# Episode Prep Command

This command updates episode documentation after completing a live session.

## Overview
Update the just-completed episode README with YouTube link, thumbnail, and summary, update the main README with episode details, and add next episode info to the table.

## Steps

1. **Check current date** - Use bash to verify today's date, run `bash(ls .)` to see the top level of folder structure here

2. **Gather required information** - ASK THE USER FOR: 
   - YouTube link to the just-completed recording
   - Summary of the just-completed episode
   - Folder for the just-completed episode (dated today or yesterday, in the past) (use List() or Bash(ls) to check if it exists)
   - Next episode signup link (starting with lu.ma/...)
   - Summary/description of the next episode

**STOP and ask the user if UNTIL YOU HAVE ALL OF THESE DATA POINTS**

3. **Update past episode meta.md**:
   - Read at least 3 other past episode meta.mds to understand the format
   - update the links and youtube url



4. **Update episode-specific README**:
   - Read 2025-07-08-context-engineering/README.md for example
   - **IMPORTANT**: Add YouTube thumbnail using this exact format (see ):
     ```markdown
     [![Episode Title](https://img.youtube.com/vi/VIDEO_ID/0.jpg)](https://www.youtube.com/watch?v=VIDEO_ID)
     ```
     Extract the VIDEO_ID from the YouTube URL (the part after v= or youtu.be/)
   - Leave whiteboards and links sections blank for manual addition
   - Navigate to the just-completed episode folder
   - Update the README with the provided summary

4a. Create a new folder for the upcoming episode following the format
   - create a new folder for the upcoming episode
   - create a meta.md, omitting the youtube links, setting url to `null` for the media section
   

```example initial meta.md
---
guid: aitw-EPISODENUMBER
title: ".."
description: |
  ..
event_link: https://luma.com/<something>
eventDate: YYYY-MM-DDT18:00:00Z
media:
  url: null
  type: video/youtube
links:
  code: https://github.com/ai-that-works/ai-that-works/tree/main/YYYY-MM-DD-<folder-name>
  # no youtube link here yet
season: 2
episode: EPISODENUMBER
event_type: episode
---
```


5. **Run the tools to regenerate the JSON manifest**
   - cd tools && bun run readme

## Important Notes
- Use TodoWrite to track progress through these steps
- Think deeply about the structure and format before making changes
- Verify all information is present before proceeding with updates
- Maintain consistency with existing episode documentation format
- The YouTube thumbnail is REQUIRED - reference 2025-07-08-context-engineering/README.md as a working example
