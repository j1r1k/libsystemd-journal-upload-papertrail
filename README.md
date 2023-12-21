# libsystemd-journal-upload-papertrail

Upload journald logs to [papertrail](http://papertrailapp.com/).

## Configuration

1. Store `PAPERTRAIL_TOKEN` in `env` file `${HOME}/.config/journal-upload-papertrail/env`

```sh
echo -n 'TOKEN' \
   | sed 's/^/:/' \
   | base64 \
   | sed 's/^/PAPERTRAIL_TOKEN=/' \
   > ~/.config/journal-upload-papertrail/env
```

2. Enable it as a systemd user service definition

```sh
cp "./systemd/journal-upload-papertrail.service" \
   "${HOME}/.config/systemd/user/journal-upload-papertrail.service"
systemctl --user daemon-reload
systemctl --user enable journal-upload-papertrail.service
systemctl --user start journal-upload-papertrail.service
```

## Notes

Upload state is stored in `${XDG_DATA_HOME}/systemd/journal-upload/state.json`. 

The uploader is resuming the logs from the cursor stored in a state file.

Depends on: 
- [libsystemd-journal-upload](https://github.com/j1r1k/libsystemd-journal-upload)
- [libsystemd-journal](https://github.com/j1r1k/libsystemd-journal)
