#!/usr/bin/env bash
# Toggle mic monitor (pw-loopback) via systemd user service

if systemctl --user is-active --quiet mic-monitor; then
    systemctl --user stop mic-monitor
else
    systemctl --user start mic-monitor
fi
