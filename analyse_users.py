from atproto import Client, client_utils
import csv
import sys
import os
import logging
from openai import OpenAI
import re
from datetime import datetime
import json
from datetime import datetime, timedelta, UTC
from dateutil.parser import isoparse

def ask_text_question(prompt, openai_client, model="gpt-4o-mini"):
    """
    Sends a text question to the OpenAI API and retrieves the response.

    Args:
        prompt (str): The question or prompt to send.
        model (str): The model to use (default: "gpt-4-mini").

    Returns:
        str: The response from the API.
    """
    try:
        response = openai_client.chat.completions.create(
            model=model,
            messages=[
                {
                    "role": "user",
                    "content": prompt,
                }
            ],
        )
        # Extract and return the response text
        return response
    except Exception as e:
        return f"Error: {e}"


def load_client():
    client = Client()
    with open("./client_session.txt", "r") as f:
        session_string = f.read().strip()
    client._import_session_string(session_string)
    return client



def generate_prompt(app_user, app_user_description, add_prompt, profile, posts, org_posts_n, posts_text):
    # if app_user == 'None':
    #     requestor_text = '''Rate how interesting this user is to follow on a scale of 1-10. Favor users with more total and engaging posts and disfavor users that mostly repost intead of original posts.'''
    #     requestor_text = ''
    else:
        requestor_text = f'''Rate how interesting this user is to follow on a scale of 1-10, based on these criteria:
                                - Does the user have many and engaging posts?
                                - Does the user post original content, and not just reposts?
                                - [Most importantly:] Does the user's interests, profession, location, and identity align well with those of the requestor? 
                                The requestor's profile name and handle is `{app_user}` and their description is:  
                                `{app_user_description}`'''
        requestor_text = re.sub('\n +', '\n', requestor_text)
    if add_prompt == 'None':
        add_prompt_add = ''
    else:
        add_prompt_add = f"In addition, consider these user-provided instructions for your ranking: {add_prompt}"
    
    prompt = f'''Below are recent posts from a BlueSky user. Analyze the information provided.
                    User Name: {getattr(profile, "display_name", 0)}
                    Profile Description: {getattr(profile, "description", "")}
                    Total number of posts and replies: {getattr(profile, 'posts_count', 0)}

                    **Task 1: User Summary**  
                    Summarize in one concise sentence who this user is & what they primarily post about.  
                    - If user is widely known (e.g., celebrity / famous company), add a second sentence starting with "To my knowledge, ..." to provide additional context.
                    - Output summary on the 1st line of your response as:  
                    `===User summary: <summary>===`  

                    **Task 2: Categorization**  
                    Classify user into one of these categories:  
                    - Academic Scientist, Industry Scientist, Politician, Scientific Organization, Public Organization, News Media, Scientific Publisher, Bot  
                    - If none of these fit, choose another category.  
                    - Output the category on the second line of your response as:  
                    `===User category: <category>===`  

                    **Task 3: Significance Rating**  
                    {add_prompt_add}
                    - Output the rating on the third line of your response as:  
                    `===User significance: <significance>, [<short justification>]===`  

                    In your response, do not provide other headers or additional text to these three output items.

                    These are the last {org_posts_n} original posts from this user, {len(posts['feed']) - org_posts_n} were removed from this collection because they were reposts.
                    {posts_text}'''
    prompt = re.sub('\n +', '\n', prompt)
    return prompt

# Configure logging
logging.basicConfig(
    filename='/Users/max/Documents/code/network_starterpacks/atproto_script.log',         # Log file name
    level=logging.INFO,            # Log level
    format='%(asctime)s - %(levelname)s - %(message)s'
)

with open('/Users/max/Documents/code/network_starterpacks/atproto_script.log', 'a') as f:
    f.write('\n\n\n')

logging.info(f"Running get_users_in_sp.py.")
# Initialize client and login
client = load_client()

def main():
    try:
        logging.info(f"sys is {','.join(sys.argv)}")    
        user_sp_data_path, user, app_user, get_posts, aisum, api_key, add_prompt, post_cutoff, sp_cutoff, fllwer_cutoff, days_cutoff, already_fl, job_id = sys.argv[1:14]
        sp_list_path = os.path.join('./', 'part_all_' + user + '.tsv')

        logging.info(f"User is {user}, app_user is {app_user} api_key is {api_key} and file is {sp_list_path} and user_sp_data_path is {user_sp_data_path}")

        if app_user == 'None':
            app_user_description = ""
        else:
            app_user_profile = client.get_profile(app_user)
            app_user = f"{app_user_profile['display_name']} ({app_user})"
            app_user_description = f"{app_user_profile['description']}"
            logging.info(f'app_user updated to {app_user}')


        if os.path.exists(user_sp_data_path):
            with open(user_sp_data_path, "r") as file:
                user_data = json.load(file)
            logging.info(f"Loaded user_data from {user_sp_data_path}")

        # Fetch profiles in batches of 25
        handles = list(user_data.keys())
        batch_size = 25 
        detailed_data = {}
        gpt_tokens = 0

        # get follows to exclude
        follows = set()
        if api_key != 'None' and already_fl == "TRUE":
            cursor = None
            while True:
                result = client.get_follows(user, limit=100, cursor=cursor)
                for user_obj in result.follows:
                    handle = user_obj.handle
                    follows.add(handle)
                cursor = result.cursor
                if cursor is None:
                    break


        for i in range(0, len(handles), batch_size):
            logging.info(f'Working on batch {i+1}-{min(i+batch_size, len(handles))}')
            batch = handles[i:i + batch_size]
            try:
                # Fetch profiles in batch
                profiles = client.get_profiles(batch)
                n = 1
                for profile in profiles['profiles']:
                    handle = getattr(profile, 'handle', 'Unknown')
                    logging.info(f"Working on profile {i + n}/{len(handles)} ({handle}, {getattr(profile, 'display_name', 0)})")
                    n += 1

                    created_at = datetime.strptime(profile.created_at, '%Y-%m-%dT%H:%M:%S.%fZ')
                    days_since_reg = (datetime.now() - created_at).days

                    logging.info(f'{profile.posts_count} >= {post_cutoff} and {profile.followers_count} >= {fllwer_cutoff} and {days_since_reg} >= {days_cutoff} and ({sp_cutoff} == "NA" or {user_data[handle]} >= int({sp_cutoff})) and {handle} not in `follows`')
                    
                    org_posts = []
                    org_posts_n = 0
                    if get_posts == "TRUE":
                        logging.info(f"Getting user posts")
                        try:
                            posts = client.get_author_feed(handle, limit=100, filter='posts_no_replies')
                            org_posts = [
                                post['post']['record']['text'] 
                                for post in posts['feed'] 
                                if post['post']['author']['handle'] == getattr(profile, 'handle', 0)
                            ]
                            org_posts_n = len(org_posts)
                            logging.info(f"Got {len(posts['feed'])} posts and {org_posts_n} org_posts.")
                            org_posts = org_posts[:35]
                            org_posts = [x.replace('\n', '||') for x in org_posts]
                        except Exception as e:
                            logging.info(f"Got {len(org_posts)} posts and some error during posts catch: {e}")
                    
                        # get recent posts
                        timestamps = [post['post']['record']['created_at'] for post in posts['feed'] if post['post']['author']['handle'] == handle]
                        now = datetime.now(UTC)
                        posts_last_month = [t for t in timestamps if isoparse(t) >= now - timedelta(days=30)]
                        posts_last_month_str = f' ({len(posts_last_month)})'
                    else:
                        posts_last_month_str = ''
                        logging.info(f"Post retrieval skipped because get_posts is {get_posts}.")


                    gpt_sum, gpt_cat, gpt_sig = "NA", "NA", "NA"

                    # perform AI summary
                    if aisum == "TRUE" and \
                       profile.posts_count >= int(post_cutoff) and \
                       profile.followers_count >= int(fllwer_cutoff) and \
                       days_since_reg >= int(days_cutoff) and \
                       (sp_cutoff == "NA" or user_data[handle] >= int(sp_cutoff)) and \
                       (already_fl == "FALSE" or handle not in follows):
                        logging.info(f'Profile matches, performing AI analysis on last {min(35, org_posts_n)} org_posts.')
                        try:
                            gpt_sum = ""
                            if api_key != 'None':
                                try:
                                    openai_client = OpenAI(api_key=api_key)
                                    posts_text = "\n\n\n".join(org_posts)

                                    prompt = generate_prompt(app_user, app_user_description, add_prompt, profile, posts, org_posts_n, posts_text)
                                    if i +n == len(handles):
                                        logging.info(prompt)

                                    gpt_sum_raw = ask_text_question(prompt, openai_client)
                                    gpt_tokens += gpt_sum_raw.usage.total_tokens
                                    gpt_sum_raw_msg = getattr(getattr(gpt_sum_raw.choices[0], 'message', '<No message>'), 'content', '<No content>')
                                    if gpt_sum_raw_msg.startswith('<'):
                                        gpt_sum, gpt_cat, gpt_sig = 'Error', 'Error', 'Error'
                                        logging.info(f"Asking gpt failed: {gpt_sum_raw}")
                                    else:
                                        gpt_cat = re.findall('(?<===User category: )[^=]*', gpt_sum_raw_msg)[0]
                                        gpt_sig = re.findall('(?<===User significance: )[^=]*', gpt_sum_raw_msg)[0]
                                        gpt_sum = re.sub('===User category: [^=]*=== *\n', '', gpt_sum_raw_msg)
                                        gpt_sum = re.sub('===User significance: [^=]*=== *\n?', '', gpt_sum)
                                        gpt_sum = re.sub('===User summary:|=== *\n?$', '', gpt_sum)
                                        logging.info(f"Successfully asked gpt [posts: {len(org_posts)}, cost: {gpt_sum_raw.usage.total_tokens} tokens] and got:\ncategory {gpt_cat} and significance {gpt_sig} and summary {gpt_sum}")
                                except Exception as e:
                                    gpt_sum = 'Error'
                                    logging.info(f"Asking gpt resulted in error: {e}")
                            else:
                                logging.info(f"ChatGPT skipped due to non existant API key")

                        except Exception as e:
                            logging.info(f"Error during AI analysis: {e}")
                    else:
                        if aisum == 'TRUE':
                            if profile.posts_count < int(post_cutoff):
                                reason = f'Profile excluded because <{post_cutoff} posts'
                            elif profile.followers_count < int(fllwer_cutoff):
                                reason = f'Profile excluded because <{fllwer_cutoff} followers'
                            elif days_since_reg < int(days_cutoff):
                                reason = f'Profile excluded because <{days_since_reg} days since registration'
                            elif(sp_cutoff != "NA" and user_data[handle] < int(sp_cutoff)):
                                reason = f'Profile excluded because <{sp_cutoff} appearance in SPs'
                            
                            elif handle in follows:
                                reason = f'Profile excluded because already followed by user'
                            else:
                                reason = 'unknown reason'
                            gpt_sum = '<i>' + reason + '</i>'
                            logging.info(f"Post analysis skipped because profile didn't match: {reason}")
                    try:
                        repost_rate = round((len(posts['feed']) - org_posts_n) / len(posts['feed']) *100, 0)
                    except Exception as e:
                        repost_rate = f'<i>NA</i>'

                    detailed_data[handle] = {
                        'name': getattr(profile, 'display_name', 0),
                        'avatar': getattr(profile, 'avatar', ''),
                        'count': user_data[handle],
                        'posts_count': f"{getattr(profile, 'posts_count', 0)}{posts_last_month_str}",
                        'repost%': repost_rate,
                        'followers_count': getattr(profile, 'followers_count', 0),
                        'follows_count': getattr(profile, 'follows_count', 0),
                        'description': (getattr(profile, 'description', '') or '').replace('\n', '||'),
                        'posts': "||--||".join(org_posts),
                        'gpt_cat': gpt_cat,
                        'gpt_sig': gpt_sig,
                        'gpt_sum': gpt_sum
                    }                        

            except Exception as e:
                logging.error(f"Error fetching profiles for batch {batch}:\n{e}")
                print(f"Error fetching profiles for batch {batch}:\n{e}")
                for handle in batch:
                    detailed_data[handle] = {
                        'name': 'Error',
                        'avatar': 'Error',
                        'count': user_data[handle],
                        'posts_count': 'Error',
                        'repost%': "NA",
                        'followers_count': 'Error',
                        'follows_count': 'Error',
                        'description': 'Error',
                        'posts': 'None',
                        'gpt_cat': 'None',
                        'gpt_sig': 'NA',
                        'gpt_sum': 'Error'
                    }

        # Save the detailed data to a TSV file
        output_file = f'{user}_user_data_detailed_{job_id}.tsv'
        with open(output_file, 'w', newline='', encoding='utf-8') as file:
            writer = csv.writer(file, delimiter='\t')
            writer.writerow(['Handle', 'Name', 'avatar', 'Count', 'Posts Count', "Repost%", 'Followers Count', 'Follows Count', 'Description', 'Posts', 'GPT_cat', 'GPT_sig', 'GPT_sum'])  # Header
            for handle, data in detailed_data.items():
                writer.writerow([handle, data['name'], data['avatar'], data['count'], data['posts_count'], data['repost%'], data['followers_count'], data['follows_count'], data['description'], data['posts'], data['gpt_cat'], data['gpt_sig'], data['gpt_sum']])

        logging.info(f"Detailed handle data of {len(detailed_data.items())} users saved to {output_file}")
        if aisum == "TRUE":
            logging.info(f'GPT requests total tokens: {gpt_tokens}, approximate price: ${round(gpt_tokens * 0.00000015, 5)}')
        return {"status": "success", "message": f"Detailed handle data saved to {output_file}", "output_file": output_file}

    except Exception as e:
        logging.error(f"Error in main(): {str(e)}")
        return {"status": "error", "message": f"Error in main(): {str(e)}"}


# if __name__ == '__main__':
#     main()
