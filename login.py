from atproto import Client
import sys
import os
import logging

with open('/Users/max/Documents/code/network_starterpacks/atproto_script.log', 'a') as f:
    f.write('\n\n\n')

# Configure logging
logging.basicConfig(
    filename='/Users/max/Documents/code/network_starterpacks/atproto_script.log',  # Log file name
    level=logging.INFO,                                                           # Log level
    format='%(asctime)s - %(levelname)s - %(message)s'
)

username = sys.argv[1]
app_password = sys.argv[2]

logging.info(f"Starting login process for user {username}.")


def load_client():
    """Load client session from file or fall back to login."""
    client = Client()
    session_file = "/Users/max/Documents/code/network_starterpacks/client_session.txt"
    
    if os.path.exists(session_file):
        logging.info("Session file found. Attempting to reuse session.")
        with open(session_file, "r") as f:
            session_string = f.read().strip()
        try:
            client._import_session_string(session_string)  # Reuse session
            test_get_follows = client.get_follows('bsky.app')  # Verify the session is valid
            if test_get_follows is None:
                raise ValueError("Session validation failed: client.me() returned None.")
            logging.info("Session is valid. Reusing existing session.")
            client.me = client._session
            return client
        except Exception as e:
            logging.warning(f"Failed to reuse session: {str(e)}. Falling back to login.")

    # If session reuse fails or file doesn't exist, perform login
    logging.info(f"Performing instead login for {username}...")
    try:
        client = Client()
        client.login(username, app_password)
        test_get_follows = client.get_follows('bsky.app')  # Verify the session is valid
        logging.info("Login successful.")
        session_string = client.export_session_string()
        with open(session_file, "w") as f:
            f.write(session_string)
        logging.info("New session saved to client_session.txt")
        return client
    except Exception as e:
        logging.warning(f"{str(e)}")
        logging.warning(f"Login failed! Check credentials?")
        return None



def main():
    client = load_client()
    if client:
        # At this point, the client is authenticated
        logging.info("Client is ready for further operations.")
        return client
    else:
        logging.info("Client is not ready.")
        return None
    
