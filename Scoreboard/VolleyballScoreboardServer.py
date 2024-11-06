# -*- coding: utf-8 -*-
import pandas as pd
from datavolley import read_dv
import os
from flask import Flask, render_template
import re

# Initialize Flask app
app = Flask(__name__)

# Assign path for DataVolley files -- TODO to be changed!
#dvw_path_folder = "C:/Users/mirko/OneDrive - Politecnico di Milano/Altro/Volley/Conco2425/Olbia/"
dvw_path_folder =  "C:/Users/mirko/Documents/GitHub/CuneoWebsite.io/Scoreboard"
file_extension = ".dvw"

# Get a list of all files with the specified extension in the directory
file_list = [f for f in os.listdir(dvw_path_folder) if f.endswith(file_extension)]
print(f"Found {len(file_list)} .dvw files.")

# Initialize an empty DataFrame to store combined data
combined_df = pd.DataFrame()

def process_file(path):
    dv_instance = read_dv.DataVolley(os.path.join(dvw_path_folder, path))
    dv_instance.home_setswon = int(dv_instance.home_setswon or 0)
    dv_instance.visiting_setswon = int(dv_instance.visiting_setswon or 0)
    df = dv_instance.get_plays()
    return df

# Process each file and combine data
for file_name in file_list:
    print(f"Processing file: {file_name}")  # Debugging line
    combined_df = pd.concat([combined_df, process_file(file_name)], ignore_index=True)

def summarize_volleyball_set(plays):
    teams = plays['team'].dropna().unique()  # Get the team names
    home_team = plays['home_team'].iloc[0]  # Home team name
    away_team = plays['visiting_team'].iloc[0]  # Away team name

    summary_data = []
    set_scores = []  # To keep track of set scores and winners
    current_set = 1  # Start with set 1

    # Variables to store current set stats
    current_timeouts = {'home': 0, 'away': 0}
    current_substitutions = {'home': 0, 'away': 0}
    #current_video_checks = {'home': 0, 'away': 0}

    home_set_wins = 0
    away_set_wins = 0

    def calculate_player_points(team):
        team_plays = plays[plays['team'] == team]
        player_points = {}
        for _, row in team_plays.iterrows():
            if row['skill'] in ['Attack', 'Serve', 'Block'] and row['evaluation_code'] == '#':
                player = row['player_name']
                if player not in player_points:
                    player_points[player] = {'Attack': 0, 'Serve': 0, 'Block': 0}
                player_points[player][row['skill']] += 1
        return player_points

    def get_team_summary(team):
        player_points = calculate_player_points(team)
        team_summary = {'team_name': team, 'players': [], 'team_total': 0}
        team_total = 0

        for player, points in player_points.items():
            total_points = points['Attack'] + points['Serve'] + points['Block']
            team_summary['players'].append({
                'player_name': player,
                'total_points': total_points,
                'attack_points': points['Attack'],
                'serve_points': points['Serve'],
                'block_points': points['Block']
            })
            team_total += total_points

        team_summary['team_total'] = team_total
        return team_summary

    for index, row in plays.iterrows():
        # Handle missing values using pd.isna
        home_score = 0 if pd.isna(row['home_team_score']) else row['home_team_score']
        away_score = 0 if pd.isna(row['visiting_team_score']) else row['visiting_team_score']

        # If either team reaches 25 or more points
        if (home_score >= 25 or away_score >= 25) and abs(home_score - away_score) >= 2:
            # Check if the next row has a reset to 0 (set end)
            if index + 1 < len(plays):
                next_row = plays.iloc[index + 1]
                next_home_score = 0 if pd.isna(next_row['home_team_score']) else next_row['home_team_score']
                next_away_score = 0 if pd.isna(next_row['visiting_team_score']) else next_row['visiting_team_score']

                if next_home_score == 0 or next_away_score == 0:
                    # The set is complete
                    set_result = f"{home_score} - {away_score}"
                    set_scores.append(set_result)

                    # Increment set count for the winning team
                    if home_score > away_score:
                        home_set_wins += 1
                    else:
                        away_set_wins += 1

                    current_set += 1  # Increment the set number

                    # Reset the stats for the next set
                    current_timeouts = {'home': 0, 'away': 0}
                    current_substitutions = {'home': 0, 'away': 0}
                    current_video_checks = {'home': 0, 'away': 0}

        # Track stats for the current set
        if row['code'].startswith('*T'):  # Home timeout
            current_timeouts['home'] += 1
        elif row['code'].startswith('aT'):  # Away timeout
            current_timeouts['away'] += 1
        elif 'VC' in row['code']:  # Video check
            if row['team'] == plays['home_team'].iloc[0]:
                current_video_checks['home'] += 1
            else:
                current_video_checks['away'] += 1
        if row['code'].startswith('*c'):  # Home timeout
            current_substitutions['home'] += 1
        elif row['code'].startswith('ac'):  # Away timeout
            current_substitutions['away'] += 1

    # Get last non-NaN score for home and away team
    final_home_score = plays['home_team_score'].dropna().tail(1).values[0] if not plays['home_team_score'].dropna().empty else 0
    print(final_home_score)
    final_away_score = plays['visiting_team_score'].dropna().tail(1).values[0] if not plays['visiting_team_score'].dropna().empty else 0

    for team in teams:
        summary_data.append(get_team_summary(team))

    # Include current set, past set scores, and stats for the dashboard
    summary_data.append({
        'current_set': current_set,
        #'team_set_wins': f"{home_team}: {home_set_wins} - {away_set_wins} {away_team}",  # Set wins string
        'team_set_wins': f"{home_set_wins} - {away_set_wins}",  # Set wins string
        'timeouts': current_timeouts,
        #'substitutions': current_substitutions,
        #'video_checks': current_video_checks,
        'past_set_scores': set_scores,
        'set_score': f"{final_home_score} - {final_away_score}"
    })

    return summary_data



def print_team_summary(team_summary):
    print(f"\n{team_summary['team_name']} Team:")
    for player in team_summary['players']:
        # Ensure Block points are always printed
        block_points = player.get('block_points', 0)  # Default to 0 if block_points is not found
        
        print(f"  {player['player_name']}: "
              f"{player['total_points']} points "
              f"(Attack: {player['attack_points']}, "
              f"Serve: {player['serve_points']}, "
              f"Block: {block_points})")  # Ensure Block points are displayed
    
    print(f"  Team Total: {team_summary['team_total']} points")


# Flask route to render the scoreboard
@app.route('/')
def show_scoreboard():
    # Generate summary data for the scoreboard
    summary_data = summarize_volleyball_set(combined_df)
    
    # Render HTML template with the summary data
    return render_template('scoreboard.html', summary_data=summary_data)

# Run the Flask app
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
