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

# Key bindings

## Ticket List

|Key  | Definition                 |
|-----|----------------------------|
| C-q | Quit                       |
| g   | Goto Ticket                |
| s   | Search by open/closed      |
| c   | Search by ticket scope     |
| l   | Search by label            |
| m   | Search by milestone        |
| a   | Search by author           |
| o   | Search by owner            |
| w   | Search by weight           |
| /   | Search by keyword          |
| C-o | Toggle Asc/Desc            |
| C-s | Toggle CreatedAt/UpdatedAt |

## Ticket Page

|Key  | Definition                 |
|-----|----------------------------|
| ESC | Return to Ticket List      |
| g   | Goto Ticket                |
| F10 | Commit ticket changes      |
| s   | Toggle Open/Close Status   |
| t   | Modify title               |
| l   | Modify labels              |
| m   | Modify milestone           |
| o   | Modify owner               |
| w   | Modify weight              |
| c   | Add a new comment          |
| d   | Modify description         |
| v   | View currently selected comment |
| n   | Move to next issue         |
| p   | Move to previous issue     |



# Building

The only tested configuration is with cabal new-build with GHC-8.6.4 and cabal-install-2.4.0.0.

```
cabal new-run
```
