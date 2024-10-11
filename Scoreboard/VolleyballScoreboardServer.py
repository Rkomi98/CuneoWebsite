# -*- coding: utf-8 -*-
import pandas as pd
from datavolley import read_dv
import os
from flask import Flask, render_template
import re

# Initialize Flask app
app = Flask(__name__)

# Assign path for DataVolley files
dvw_path_folder = "C:/Users/mirko/OneDrive - Politecnico di Milano/Altro/Volley/Conco2425/Olbia/"
file_extension = ".dvw"

# Get a list of all files with the specified extension in the directory
file_list = [f for f in os.listdir(dvw_path_folder) if f.endswith(file_extension)]
print(f"Found {len(file_list)} .dvw files.")

# Initialize an empty DataFrame to store combined data
combined_df = pd.DataFrame()

def process_file(path):
    dv_instance = read_dv.DataVolley(os.path.join(dvw_path_folder, path))
    df = dv_instance.get_plays()
    return df

# Process each file and combine data
for file_name in file_list:
    print(f"Processing file: {file_name}")  # Debugging line
    combined_df = pd.concat([combined_df, process_file(file_name)], ignore_index=True)

# Summarize volleyball set and return data for rendering
def summarize_volleyball_set(plays):
    teams = plays['team'].dropna().unique()
    summary_data = []
    set_scores = {}  # To keep track of set scores and winners

    def calculate_player_points(team):
        team_plays = plays[plays['team'] == team]
        player_points = {}
        for _, row in team_plays.iterrows():
            if row['skill'] in ['Attack', 'Serve'] and row['evaluation_code'] == '#':
                player = row['player_name']
                if player not in player_points:
                    player_points[player] = {'Attack': 0, 'Serve': 0}
                player_points[player][row['skill']] += 1
        return player_points

    def get_team_summary(team):
        player_points = calculate_player_points(team)
        team_summary = {'team_name': team, 'players': [], 'team_total': 0}
        team_total = 0

        for player, points in player_points.items():
            total_points = points['Attack'] + points['Serve']
            team_summary['players'].append({
                'player_name': player,
                'total_points': total_points,
                'attack_points': points['Attack'],
                'serve_points': points['Serve']
            })
            team_total += total_points

        team_summary['team_total'] = team_total
        return team_summary

    # Count timeouts, substitutions, and video checks
    timeouts_called = len(plays[plays['code'].str.contains('TO', na=False)])
    substitutions_done = len(plays[plays['code'].str.contains('SUB', na=False)])
    video_checks_called = len(plays[plays['code'].str.contains('VC', na=False)])

    # Identify set results from the code column
    for index, row in plays.iterrows():
        # Check if the code contains "set" (indicating a set completion)
        if re.match(r'^\d+set$', row['code']):
            set_number = int(row['code'][0])  # Extract the number before 'set'
            winning_team = row['team']

            # Look for the previous row to extract the score
            if index > 0:
                prev_row = plays.iloc[index - 1]
                if '*p' in prev_row['code']:
                    score_str = prev_row['code']
                    # Extract scores
                    scores = score_str.split(':')[1]  # "25:20" => "20"
                    home_score, away_score = map(int, scores.split(':'))
                    # Store set score results
                    set_scores[set_number] = {
                        'winning_team': winning_team,
                        'score': f"{home_score}-{away_score}"
                    }

    # Get the last completed set number
    last_set_number = max(set_scores.keys(), default=0)

    # Get last non-NaN score for home and away team
    home_score = plays['home_team_score'].dropna().tail(1).values[0] if not plays['home_team_score'].dropna().empty else 0
    away_score = plays['visiting_team_score'].dropna().tail(1).values[0] if not plays['visiting_team_score'].dropna().empty else 0

    for team in teams:
        summary_data.append(get_team_summary(team))

    # Include set scores and other details in the summary
    summary_data.append({
        'set_score': f"{home_score} - {away_score}",  # Match score
        'last_set_number': last_set_number,  # Last set number played
        'timeouts_called': timeouts_called,
        'substitutions_done': substitutions_done,
        'video_checks_called': video_checks_called,
        'set_scores': set_scores  # Store all set scores
    })

    return summary_data


# Flask route to render the scoreboard
@app.route('/')
def show_scoreboard():
    # Generate summary data for the scoreboard
    summary_data = summarize_volleyball_set(combined_df)
    
    # Render HTML template with the summary data
    return render_template('scoreboard.html', summary_data=summary_data)

# Run the Flask app
if __name__ == '__main__':
    app.run(debug=True)
