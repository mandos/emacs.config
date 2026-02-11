# emacs.config
Doom Emacs private configuration

## Setup Instructions

### Initial Configuration

1. **Copy the configuration template:**
   ```bash
   cp config.local.example.el config.local.el
   ```

2. **Add your credentials to config.local.el:**
   - Open `config.local.el` in your editor
   - Fill in your name and email address
   - Add Google Calendar credentials (see instructions in the file)

3. **Important:**
   - `config.local.el` is automatically ignored by git
   - Never commit this file to version control
   - It's safe to share your Emacs config publicly without this file

### Google Calendar Setup (org-gcal)

To use org-gcal integration:

1. Create a project in [Google Cloud Console](https://console.cloud.google.com)
2. Enable the Google Calendar API
3. Create OAuth 2.0 Desktop Application credentials
4. Copy your Client ID and Secret to `config.local.el`
5. Run `M-x org-gcal-fetch` to authorize and sync

See [org-gcal documentation](https://github.com/kidd/org-gcal.el) for detailed setup.

## Usage

After setup, run `doom sync` and restart Emacs. Your personal configuration will be loaded automatically from `config.local.el`.
