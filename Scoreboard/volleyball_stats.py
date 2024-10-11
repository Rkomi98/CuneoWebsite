# -*- coding: utf-8 -*-
"""
Created on Fri Oct 11 19:07:25 2024

@author: mirko
"""

import pandas as pd
from datavolley import read_dv
import os
import numpy as np

def parse_dvw_file(file_path):
    match = read_dv(file_path)
    return match

# Assign path
dvw_path_folder = "C:/Users/mirko/OneDrive - Politecnico di Milano/Altro/Volley/Conco2425/Olbia/"
#&10_IMD-VOL.dvw
file_extension = ".dvw"

# Get a list of all files with the specified extension in the directory
file_list = [f for f in os.listdir(dvw_path_folder) if f.endswith(file_extension)]

# Initialize an empty DataFrame to store combined data
combined_df = pd.DataFrame()

def process_file(path):
    dv_instance = read_dv.DataVolley(os.path.join(dvw_path_folder, path))
    df = dv_instance.get_plays()
    return df

# Loop through each file path
for file_name in file_list:
    combined_df = pd.concat([combined_df, process_file(file_name)], ignore_index=True)

# Filter for attacks and print attacks 
print(
    combined_df[combined_df['skill'] == 'Attack']
    .groupby(['player_name', 'team'])
    .agg(Att=('skill', 'count'),
         K=('evaluation_code', lambda x: x.eq('#').sum(skipna=True)),
         K_pct=('evaluation_code', lambda x: round((x.eq('#').sum(skipna=True) / (x.count() or np.nan)), 3)))  # Use np.nan instead of np.NaN
    .reset_index()
    .sort_values(by='K_pct', ascending=False)
    .head(20)
    .reset_index(drop=True)
    .to_string()
)

def summarize_volleyball_set(plays):
    # Ensure teams are unique and non-NaN
    teams = plays['team'].dropna().unique()
    
    # Function to calculate points per player for a specific team
    def calculate_player_points(team):
        # Filter the plays for the given team
        team_plays = plays[plays['team'] == team]
        player_points = {}

        # Loop through plays and count points
        for _, row in team_plays.iterrows():
            # Check if it's an Attack or Serve point with evaluation '#'
            if row['skill'] in ['Attack', 'Serve'] and row['evaluation_code'] == '#':
                player = row['player_name']
                # Initialize the player's point dictionary if not already present
                if player not in player_points:
                    player_points[player] = {'Attack': 0, 'Serve': 0}
                # Increment the appropriate skill (Attack or Serve)
                player_points[player][row['skill']] += 1

        return player_points

    # Function to print the summary for each team
    def print_team_summary(team):
        print(f"\n{team} Team:")
        player_points = calculate_player_points(team)
        print(player_points)
        
        # Print points for each player
        for player, points in player_points.items():
            total_points = points['Attack'] + points['Serve']
            print(f"  {player}: {total_points} points (Attack: {points['Attack']}, Serve: {points['Serve']})")
        
        # Calculate total points for the team
        team_total = sum(points['Attack'] + points['Serve'] for points in player_points.values())
        print(f"  Team Total: {team_total} points")
    
    # Function to calculate and print the set score
    def calculate_set_score():
        home_team, away_team = teams
        
        # Get the last non-NaN value for home_team_score
        home_score = plays['home_team_score'].dropna().tail(1).values[0]
        
        # Get the last non-NaN value for visiting_team_score
        away_score = plays['visiting_team_score'].dropna().tail(1).values[0]
        
        return f"{home_score}-{away_score}"

    # Print summaries for both teams
    for team in teams:
        print_team_summary(team)

    # Print the set score
    print("\nSet Score:")
    print(calculate_set_score())
    
summarize_volleyball_set(combined_df)