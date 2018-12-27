This tool is aimed at power-users who want a more intuitive and focused
user interface for gitlab issues. The goal is that it should be ergonomic to
process and read issues without using the web interface.



Features

* Compact terminal interface to GitLab
* Quickly assign labels, comment and other metainformation to issues.
* Advanced ticket querying along multiple axis
* Search refinement

TODO

1. Implement remaining ticket edit options
  * milestone - unsure about the format here
  * labels    - need to lookup labels but they are just text
      *  https://docs.gitlab.com/ee/api/labels.html
    - Need to implement autocomplete widget really
  * weight  -- Just an Int
  * owners - Need to lookup user to get ID -- findUserByUsername, done

2. Ticket filtering
  * State
  * label
  * milestone
  * author
  * owner
  * weight
  * sorting
  * search

