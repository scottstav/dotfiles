# Notification Styling Design: Glass + Motion

## Goal

Upgrade SwayNC notification styling to a premium glass-morphism aesthetic with tasteful GTK4 CSS animations. Unified style across all notification sources (Claude, calendar, system, voice).

## Approach: Glass + Motion

Frosted glass foundations (layered shadows, gradient accents, weight-varied typography, pill buttons) with tasteful motion on top (arrival glow fade, hover lift, smooth button states).

## Color & Shadow System

Palette unchanged: `#89cff0` light blue accent, dark grays, Iosevka monospace.

3-layer shadow on every notification card:
1. Inner glow: `inset 0 1px 0 rgba(255,255,255,0.04)` — glass edge highlight
2. Drop shadow: `0 8px 32px rgba(0,0,0,0.4)` — depth
3. Ambient light: `0 0 1px rgba(137,207,240,0.12)` — accent halo

Border: `rgba(137, 207, 240, 0.3)` — slightly more visible than current.

## Typography

- Summary: Iosevka 14px bold
- Body: Iosevka 12.5px regular, `#c8c8c8`
- Timestamp: Iosevka 10px, accent color

## Notification Cards

- Background: `rgba(40, 40, 45, 0.88)`
- Border-radius: 14px
- Inner padding: 14px
- Hover: lighten bg, intensify border, deepen shadow, `transform: scale(1.01)`

## Animations (GTK4 @keyframes)

- `notification-arrive`: border glows accent blue for 400ms, fades back
- Hover lift: 200ms ease on transform + box-shadow + background
- Action buttons: scale 0.97 → 1.0 on hover, 120ms ease-out

## Action Buttons

- Pill-shaped: `border-radius: 20px`
- Default: `rgba(80, 80, 80, 0.4)`, no border
- Hover: accent background, slight scale-up, 150ms transition

## Close Button

- 20px (down from 24px), more transparent default
- Hover: accent color (not crimson)

## Critical Notifications

- Left border: 3px solid urgent-color
- Red ambient shadow: `0 0 16px rgba(220, 20, 60, 0.2)`
- Arrival animation pulses red instead of blue

## Floating Notifications

- Stronger shadow: `0 12px 40px rgba(0,0,0,0.5)`
- Border-radius: 16px

## Control Center

- Matching glass treatment, border-radius 16px
- Widget cards get consistent rounded styling

## Config

- transition-time: 250ms (up from 200)
