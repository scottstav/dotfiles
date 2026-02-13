# Editor

When asked to open a file, use `emacsclient -c +LINE FILE` to open it in a new Emacs frame. Replace `LINE` with the target line number and `FILE` with the file path.

# Denote Journal

When asked to add something to the journal, write to today's Denote journal file. Use `emacsclient --eval` to call Denote's elisp functions so file naming and creation stays in sync with the user's Emacs config.

## Getting today's journal file path

```sh
emacsclient --eval '(denote-journal-path-to-new-or-existing-entry)'
```

This returns the file path as a quoted string. It creates the entry if one doesn't exist for today, and doesn't switch buffers. Strip the surrounding quotes from the returned path before using it.

For the **work journal**, override the directory and title:

```sh
emacsclient --eval '(let ((denote-journal-directory (expand-file-name "~/Dropbox/org/denote/work/journal")) (denote-journal-file-name-title "work")) (denote-journal-path-to-new-or-existing-entry))'
```

Use the work journal when the context is clearly work-related.

## Writing content

Once you have the file path, read the file and append content using the normal Edit tool.

- Use org-mode headings (`* Heading`) for distinct topics
- Keep the tone consistent with existing entries (casual, concise)

# Connecting to MongoDB via AWS

When asked to connect to a service's database, use the pattern below.

- **Service name** = name of the current working directory (e.g. `ai-media-svc`)
- **Environment** = ask the user if not specified (test, dev, production)
- **AWS account ID** = only needed if profiles don't exist yet; ask the user

## AWS profile setup

Each service needs two profiles in `~/.aws/config`. Check if they already exist first. If not, add them using this pattern (reference: existing `user-equipment-svc` profiles):

```ini
[profile own-op-<service>-<env>]
sso_start_url = https://ifitsso.awsapps.com/start
sso_region = us-east-1
sso_account_id = <account-id>
sso_role_name = own-op-<service>-<env>
region = us-east-1

[profile <service>-<env>-role]
role_arn = arn:aws:iam::<account-id>:role/<service>-iac-role-<env>
source_profile = own-op-<service>-<env>
role_session_name = scotts@ifit.com
region = us-east-1
```

Note: `<env>` is `test`, `dev`, or `prod` everywhere EXCEPT in the IAC role ARN, where prod becomes `production` (e.g. `<service>-iac-role-production`). All other references use `prod`.

## Fetching credentials and connecting

```sh
# 1. Fetch DB credentials from SSM
MONGO_AUTH=$(aws-vault exec <service>-<env>-role -- aws ssm get-parameter \
  --name "/secrets/<service>/<env>/db-auth" \
  --with-decryption \
  --query "Parameter.Value" \
  --output text)

# 2. Build connection string (MONGO_AUTH contains "user:password@")
MONGO_URI="mongodb+srv://${MONGO_AUTH}<host>/<database>?retryWrites=true&w=majority"

# 3. Connect
mongosh "$MONGO_URI"
```

The MongoDB host and database name can be found in the service's Terraform variables for the given environment (e.g. `terraform/envs/<env>/`). If not found there, check config files or env vars. Ask the user as a last resort.

# Agents and Commands

After creating or modifying a file in `~/.claude/agents/` or `~/.claude/commands/`, run `~/dotfiles/sync-claude.sh` to adopt it into the dotfiles repo and symlink it back. This ensures new agents and skills are version-controlled.
