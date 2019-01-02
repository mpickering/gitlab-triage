This tool is aimed at power-users who want a more intuitive and focused
user interface for gitlab issues. The goal is that it should be ergonomic to
process and read issues without using the web interface.

More features and modes will be developed as I use them or perhaps not at all.

Features

* Compact terminal interface to GitLab
* Quickly assign labels, comment and other metainformation to issues.
* Advanced ticket querying along multiple axes

# Screenshots

<img src="https://i.imgur.com/16U6sqi.png" width=50%>
<img src="https://i.imgur.com/p10H3am.png" width=50%>

# Building

The only tested configuration is with nix, installation with cabal and stack
will be supported in the future.

```
nix-shell
cabal run
```
