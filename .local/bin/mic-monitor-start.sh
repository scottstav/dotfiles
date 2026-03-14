#!/usr/bin/env bash
# Resolve current default source/sink and start pw-loopback targeting them.
# Called by mic-monitor.service — not meant to be run directly.

SOURCE=$(wpctl inspect @DEFAULT_SOURCE@ | grep 'node.name' | head -1 | sed 's/.*"\(.*\)"/\1/')
SINK=$(wpctl inspect @DEFAULT_SINK@ | grep 'node.name' | head -1 | sed 's/.*"\(.*\)"/\1/')

if [ -z "$SOURCE" ] || [ -z "$SINK" ]; then
    echo "Failed to resolve default source/sink" >&2
    exit 1
fi

exec pw-loopback \
    --capture-props="target.object=$SOURCE" \
    --playback-props="target.object=$SINK" \
    --latency=512/48000
