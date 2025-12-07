_:

{
  # macOS Spotlight does not index symbolic links, so apps installed by Home Manager
  # are not discoverable via Spotlight search by default. This option copies the .app
  # bundles to ~/Applications instead of symlinking them, making them visible to Spotlight.
  targets.darwin.copyApps.enable = true;
}
