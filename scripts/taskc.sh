#!/bin/bash

# Get current time components (force base 10)
current_hour=$((10#$(date +%H)))
current_minute=$((10#$(date +%M)))

# Determine the most recent hour or half hour
if [ $current_minute -lt 30 ]; then
    # If current minute is less than 30, go back to the top of current hour
    start_hour=$current_hour
    start_minute=0
else
    # If current minute is 30 or more, start from 30 minutes past current hour
    start_hour=$current_hour
    start_minute=30
fi

# Function to format time in military format
format_task_time() {
    local hour=$1
    local minute=$2
    printf "%02d:%02d - Task - 30 min\n" $hour $minute
}

# Generate times for next 6 hours (12 half-hour increments)
hour=$start_hour
minute=$start_minute

for i in {1..12}; do
    format_task_time $hour $minute

    # Increment by 30 minutes
    minute=$((minute + 30))

    # Handle minute overflow
    if [ $minute -ge 60 ]; then
        minute=$((minute - 60))
        hour=$((hour + 1))
    fi

    # Handle hour overflow (24-hour format)
    if [ $hour -ge 24 ]; then
        hour=0
    fi
done