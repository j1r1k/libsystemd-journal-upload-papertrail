# libsystemd-journal-papertrail

Upload journald logs to [papertrail](http://papertrailapp.com/).

## Configuration

1. Store `PAPERTRAIL_TOKEN` in `env` file `${HOME}/.config/journal-upload/papertrail/env`

```sh
echo "PAPERTRAIL_TOKEN=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" > ~/.config/journal-upload-papertrail/env
```

2. Enable it as a systemd user service definition

```sh
cp "./systemd/journal-upload-papertrail.service" \
   "${HOME}/.config/systemd/user/journal-upload-papertrail.service"
systemctl --user daemon-reload
systemctl --user enable journal-upload-papertrail.service
systemctl --user start journal-upload-papertrail.service
```