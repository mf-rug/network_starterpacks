from atproto import Client, client_utils
import csv
import sys
import time
import logging

# Configure logging
logging.basicConfig(
    filename='/Users/max/Documents/code/network_starterpacks/atproto_script.log',         # Log file name
    level=logging.INFO,            # Log level
    format='%(asctime)s - %(levelname)s - %(message)s'
)

logging.info(f"Running process_25_follows.py.")

def load_client():
    client = Client()
    with open("/Users/max/Documents/code/network_starterpacks/client_session.txt", "r") as f:
        session_string = f.read().strip()
    client._import_session_string(session_string)
    return client


# Initialize client and login
client = load_client()

user_list_path = sys.argv[1]  # Path to the user list file
centered_user = sys.argv[2]  # Path to the user list file
start_line = int(sys.argv[3])  # Starting line of the batch
end_line = int(sys.argv[4])  # Ending line of the batch

# Helper function for rate-limited API requests
def make_request_with_backoff(request_func, max_retries=5, initial_delay=0, base_sleep=0.01, global_delay=0.02):
    delay = initial_delay
    for attempt in range(max_retries):
        try:
            result = request_func()
            if base_sleep > 0:
                time.sleep(base_sleep)
            if global_delay > 0:
                time.sleep(global_delay)  # Add delay between calls
            return result
        except Exception as e:
            if 'RateLimitExceeded' in str(e):
                logging.info(f"Rate limit exceeded for {request_func}.")
                if attempt < max_retries - 1:
                    logging.info(f"Waiting {delay} seconds before retry...")
                    time.sleep(delay)
                    delay *= 2  # Exponential backoff
                else:
                    error_message = f"Max retries exceeded after rate limiting: {str(e)}"
                    logging.error(error_message)
                    return {"status": "error", "message": error_message}
            else:
                error_message = f"Request failed: {str(e)}"
                logging.error(error_message)
                return {"status": "error", "message": error_message}

def fetch_starter_packs(client, actor):
    try:
        logging.info(f"Fetching starter packs for {actor}...")
        params = client_utils.text_builder.models.AppBskyGraphGetActorStarterPacks.Params(actor=actor)
        response = make_request_with_backoff(lambda: client.app.bsky.graph.get_actor_starter_packs(params))
        
        if isinstance(response, dict) and response.get("status") == "error":
            # Propagate the error response
            return response
        
        starter_packs = response.starter_packs
        logging.info(f"Found {len(starter_packs)} starter packs for {actor}.")
        return {"status": "success", "starter_packs": starter_packs}
    except Exception as e:
        error_message = f"Error fetching starter packs for {actor}: {str(e)}"
        logging.error(error_message)
        return {"status": "error", "message": error_message}



def main():
    try:
        # Read the input file and extract the relevant range of users
        with open(user_list_path, 'r', newline='', encoding='utf-8') as file:
            reader = csv.DictReader(file, delimiter='\t')  # Read as dictionary for column names
            rows = list(reader)  # Convert to list for slicing

        # Ensure the specified range is valid
        if start_line < 1 or end_line > len(rows):
            raise ValueError(f"Invalid line range: {start_line}-{end_line}. File has {len(rows)} lines.")

        # Slice the rows for the specified range
        users_to_process = rows[start_line - 1:end_line]  # Adjust for 0-based indexing

        logging.info(f"Processing lines {start_line} to {end_line} ({len(users_to_process)})")
        
        # Initialize a list to hold all data
        all_data = []

        for user in users_to_process:
            user_type = user['Type']
            handle = user['Handle']
            display_name = user['Display Name']

            logging.info(f"Processing user {handle} ({user_type})")

            # Fetch starter packs for the user
            try:
                result = fetch_starter_packs(client, handle)
                if result.get("status") == "error":
                    raise RuntimeError(f"Error fetching starter packs for {handle}: {result['message']}")

                starter_packs = result['starter_packs']

                # Prepare rows for this user
                for pack in starter_packs:
                    # logging.info(f"Starter Pack Raw: {pack}")
                    if hasattr(pack, 'record') and hasattr(pack.record, 'name'):
                        pack_name = pack.record.name
                    else:
                        pack_name = "Unnamed"
                    short_id = pack.uri.split('/')[-1]
                    url = f"https://bsky.app/starter-pack/{handle}/{short_id}"
                    logging.info(f"Starter Pack Found: {pack_name} ({url})")
                    all_data.append([user_type, handle, display_name, pack_name, url])

            except Exception as e:
                logging.error(f"Error processing user {handle}: {str(e)}")
                raise RuntimeError(f"Error processing user {handle}: {str(e)}")

        # Write all collected data to a single file
        if all_data:
            output_file = f'./part_{start_line}-{end_line}_{centered_user}.tsv'
            logging.info(f"Saving to {output_file}")
            with open(output_file, 'w', newline='', encoding='utf-8') as file:
                writer = csv.writer(file, delimiter='\t')
                writer.writerow(['Type', 'Handle', 'Display Name', 'Starter Pack Name', 'URL'])  # Headers
                writer.writerows(all_data)

            logging.info(f"Data saved to {output_file}")
            return {"status": "success", "output_file": output_file}
        else:
            logging.info("No starter packs found in this range.")
            return {"status": "success. No starter packs found."}
       

    except Exception as e:
        logging.error(f"Error in main(): {str(e)}")
        return {"status": "error", "message": f"Error in main(): {str(e)}"}


