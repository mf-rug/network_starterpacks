from atproto import Client
import csv
import sys
import time
import re
import logging

# Configure logging
logging.basicConfig(
    filename='/Users/max/Documents/code/network_starterpacks/atproto_script.log',         # Log file name
    level=logging.INFO,            # Log level
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logging.info(f"Running now get_follows.py.")

def load_client():
    client = Client()
    with open("/Users/max/Documents/code/network_starterpacks/client_session.txt", "r") as f:
        session_string = f.read().strip()
    client._import_session_string(session_string)
    return client

# Helper function for rate-limited API requests
def make_request_with_backoff(request_func, max_retries=5, initial_delay=0, base_sleep=0.01, global_delay=0.02):
    logging.info(f"Logging backoff for {request_func}.")
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
                if attempt < max_retries - 1:
                    logging.info(f"Rate limit exceeded. Waiting {delay} seconds before retry...")
                    time.sleep(delay)
                    delay *= 2  # Exponential backoff
                else:
                    logging.error(f"Max retries exceeded after rate limiting: {str(e)}")
                    return {"status": "error", "message": f"Rate limit exceeded: {str(e)}"}
            else:
                logging.error(f"Error during request: {str(e)}")
                return {"status": "error", "message": str(e)}

# Fetch followers and follows
def get_all_followers_and_follows(client, user,get_flers, get_fls, aslist=False):
    followers = set()  # Use sets for fast lookup
    follows = set()

    try:
        if get_flers == 'TRUE':
            logging.info("Fetching followers..")
            cursor = None
            while True:
                result = make_request_with_backoff(lambda: client.get_followers(user, limit=100, cursor=cursor))
                if isinstance(result, dict) and result.get("status") == "error":
                    return result  # Return error details

                for user_obj in result.followers:
                    handle = user_obj.handle
                    avatar = getattr(user_obj, 'avatar', '')
                    display_name = getattr(user_obj, 'display_name', '')
                    description = getattr(user_obj, 'description', '')
                    if description:
                        description = re.sub('\r?\n', '||', description)
                    created = getattr(user_obj, 'created_at', '')
                    associated = getattr(user_obj, 'associated', None)
                    starter_packs = getattr(associated, 'starter_packs', None) if associated else None
                    followers.add((handle, avatar, display_name, description, created, starter_packs))
                cursor = result.cursor
                if cursor is None:
                    break

        if get_fls == 'TRUE':
            logging.info("Fetching follows..")
            cursor = None
            while True:
                result = make_request_with_backoff(lambda: client.get_follows(user, limit=100, cursor=cursor))
                if isinstance(result, dict) and result.get("status") == "error":
                    return result  # Return error details

                for user_obj in result.follows:
                    handle = user_obj.handle
                    avatar = getattr(user_obj, 'avatar', '')
                    display_name = getattr(user_obj, 'display_name', '')
                    description = getattr(user_obj, 'description', '')
                    if description:
                        description = re.sub('\r?\n', '||', description)
                    created = getattr(user_obj, 'created_at', '')
                    associated = getattr(user_obj, 'associated', None)
                    starter_packs = getattr(associated, 'starter_packs', None) if associated else None
                    follows.add((handle, avatar, display_name, description, created, starter_packs))
                cursor = result.cursor
                if cursor is None:
                    break

        if aslist:
            return {"status": "success", "followers": list(followers), "follows": list(follows)}
        else:
            return {"status": "success", "followers": followers, "follows": follows}
    except Exception as e:
        logging.error(f"Error fetching followers/follows: {str(e)}")
        return {"status": "error", "message": f"Error fetching followers/follows: {str(e)}"}


def main():

    logging.info(f"loading client")
    client = load_client()

    # Initialize client and login
    logging.info(f"User check")
    user = sys.argv[1]
    get_follows = sys.argv[2]
    get_followers = sys.argv[3]

    logging.info(f"User is {user} and get_follows is {get_follows} and get_followers is {get_followers}")

    try:
        # get SPs for user themselves
        self_user_obj = client.get_profile(user)
        self_display_name = getattr(self_user_obj, 'display_name', '')
        self_avatar = getattr(self_user_obj, 'avatar', '')
        self_description = getattr(self_user_obj, 'description', '')
        if self_description:
            self_description = re.sub('\r?\n', '||', self_description)
        self_created = getattr(self_user_obj, 'created_at', '')
        self_associated = getattr(self_user_obj, 'associated', None)
        self_starter_packs = getattr(self_associated, 'starter_packs', None) if self_associated else None  

        # Fetch followers and follows
        result = get_all_followers_and_follows(client, user, get_followers, get_follows)
        if result.get("status") == "error":
            return result  # Propagate error details to R

        followers = result["followers"]
        follows = result["follows"]

        # Identify mutual connections
        mutuals = {f for f in followers if f in follows}
        unique_followers = followers - mutuals
        unique_follows = follows - mutuals

        # Save to file
        output_file = f'./flwer_data_{user}.tsv'
        with open(output_file, 'w', newline='', encoding='utf-8') as file:
            writer = csv.writer(file, delimiter='\t')
            writer.writerow(['Type', 'Handle', 'avatar', 'Display Name', 'Description', 'Created', 'Starter Packs'])  # Headers
            writer.writerow(['User', user, self_avatar, self_display_name, self_description, self_created, self_starter_packs or ''])
            for handle, avatar, display_name, description, created, starter_packs in unique_followers:
                writer.writerow(['Follower', handle, avatar, display_name, description, created, starter_packs or ''])
            for handle, avatar, display_name, description, created, starter_packs in unique_follows:
                writer.writerow(['Follow', handle, avatar, display_name, description, created, starter_packs or ''])
            for handle, avatar, display_name, description, created, starter_packs in mutuals:
                writer.writerow(['Mutual', handle, avatar, display_name, description, created, starter_packs or ''])

        logging.info(f"User list saved to {output_file}")
        logging.info(f"Self: {(user, self_display_name, self_starter_packs)}\nFollows: {unique_follows}\nFollowers: {unique_followers}")
        logging.info(f"Total followers: {len(followers)}, Total follows: {len(follows)}, Total mutuals: {len(mutuals)}")

        return {"status": "success", "output_file": output_file}

    except Exception as e:
        logging.error(f"Error in main(): {str(e)}")
        return {"status": "error", "message": f"Error in main(): {str(e)}"}

# if __name__ == "__main__":
#     main()