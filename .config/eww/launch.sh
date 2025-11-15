#!/bin/bash

# Control Center launcher script for eww

# Function to open control center with animation
open_control_center() {
    echo "Opening Control Center..."
    
    # Close if already open
    eww close control_center 2>/dev/null
    
    # Set closed state initially
    eww update control_center_open=false
    
    # Open the window
    eww open control_center
    
    # Small delay then trigger open animation
    sleep 0.1
    eww update control_center_open=true
}

# Function to close control center with animation
close_control_center() {
    echo "Closing Control Center..."
    
    # Trigger close animation
    eww update control_center_open=false
    
    # Wait for animation then close window
    sleep 0.3
    eww close control_center
}

# Function to toggle control center
toggle_control_center() {
    if eww active-windows | grep -q "control_center"; then
        close_control_center
    else
        open_control_center
    fi
}

# Main script logic
case "$1" in
    "open")
        open_control_center
        ;;
    "close")
        close_control_center
        ;;
    "toggle"|*)
        toggle_control_center
        ;;
esac