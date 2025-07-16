# cariandrum22's dotfiles

## Installation

```shell
git clone https://github.com/cariandrum22/dotfiles && cd dotfiles && bash setup.sh
```

### Note

This repository contains the iTerm2 configuration, which is not deployed by the setup script.

- It does not make sense to use symbolic links for iTerm2 configuration files
  - The configuration file in the default path will be replaced by the regular file when the
    configuration is changed.
  - If you give a symbolic link as a custom folder, it will be set to the path that resolves the
    link.

Therefore, to manage your iTerm2 settings in this repository, you need to check
`Load preferences from a custom folder or URL` in `Preferences > General Preferences` in iTerm2 and
then load the `Library/Preferences/com.googlecode.iterm2.plist` in this repository.
