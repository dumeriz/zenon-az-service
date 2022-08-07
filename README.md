# zenon-az

A service providing information and updates about Zenon Accelerator-Z proposals and updates.

This service is the data backend for the telegram channel https://t.me/acceleratorz_updates. It does not contain the bot implementation, for that, see https://github.com/dumeriz/zaz_telegram_py, which is using this service.

Two ZMQ endpoints are provided:
  - A REQ socket, to answer queries about proposals
  - A PUB socket, publishing updates on proposals and regular statistics on Pillar participation.

State is managed in an in-memory db, which is simply a hash-table.  As there is no subscription api for the accelerator contract in znnd, 
the service polls the `getAll`-endpoint periodically and generates a diff with the last known state. Changes are persisted and published as json data.  Periodically, statistics about pillar participation rates are generated and also published.

The service writes to two locations on disk:
  - `~/.local/share/zaz`: A backup file `backup.cls` is written here before each update
  - `~/.cache/zaz/log`: For rolling logfile.

Those locations are configurable, see parameters in `projects-db.lisp`.

## Status
Stable; working reliably for a few months now without problems. There's some code deficiencies like unimplemented but exported symbols, see (#Todo).

## Usage
This service is the data backend for the accelerator-z telegram channel. The endpoints are not public, and probably won't be, because I don't want to have to deal with rogue clients. However, if you want to access them, e.g. to implement bots for other platfroms, let me know and I can set up an instance for you. You can also self host:

### Self hosting
To run the service on Linux, install [sbcl](https://www.sbcl.org) and the zeromq dev-package.
```sh
sudo apt install sbcl
sudo apt install libzmq3-dev
```

For dependency and system loading, get the [Zenon-API-SDK for Common Lisp](https://github.com/dumeriz/cl-zenon) and the [quicklisp package manager](https://www.quicklisp.org/beta/). For quicklisp:
```sh
curl -O https://beta.quicklisp.org/quicklisp.lisp
# see website for gpg validation
```
Spin up a lisp in the same directory by typing `sbcl`. In the REPL:
```common-lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```
Next time you start sbcl `quicklisp` will be available for package management.

To be able to load this system, link it and its direct dependency to quicklisp's `local-projects`:
```sh
ln -s /full/path/to/cl-zenon ~/quicklisp/local-projects/
ln -s /full/path/to/zenon-az-service ~/quicklisp/local-projects/
```

You should be ready to start the service now. In a REPL:
```common-lisp
(ql:quickload :zaz)
(defparameter *public-znn-node* "<put a ws-node address here>")
(defparameter *polling-rate* 15)
(defparameter *rep* 45678)
(defparameter *pub* 45679)
(zaz:main *public-znn-node* :cycle-minutes *polling-rate* :pub *pub* :rep *rep*)
```
This should start the service with a refresh rate of 15 minutes. It also prints out the bound ports.
If you don't provide both `:pub` and `:rep` keys, random ports will be chosen. Query them with
```common-lisp
(z/ch:port (z/chan::reply-port))
(z/ch:port (z/chan::pub-port))
```
Don't forget to open the firewall if you want to access the service remotely.

### Available Requests
All replies are sent as json strings.

  - `"last-updates"` with parameter `n <= 50`: Sends the timestamps of the recent `n` updates
  - `"projects"` replies with all existing projects (similar to `getAll` but with other format)
  - `"active-projects"` replies with the subset of projects that need voting or are active
  - `"project-current-phase"` expects a project id and sends the current phase of that project.

### Sent updates
All updates are single-part messages in json format with the common fields `"type"` and `"timestamp"`. Available types:

  - `"project:new"`
  - `"project:votes-update"`
  - `"project:status-update"`
  - `"phase:new"`
  - `"phase:votes-update"`
  - `"phase:status-update"`
  - `"phase:update"` (when a phase was reset)

The system maintains a database of the most recent updates (default `50`; change the constant `+max-updates+` in `projects-db.lisp` to change that) and the current project state. Both are stored in a single file in (by default, see `*data-dir*`, `~/.local/share/backup.cls`). But you don't normally have to access that file manually. Here are some available functions for state inspection:
```common-lisp
;; get a list of the active projects with a specific name from the db
(z/db:get-project-by-name "Zenon Java SDK")
;; check its properties using describe, e.g.:
(describe (first *))
;; print the json of the last 5 sent updates
(dolist (i '(alexandria:iota 5 :start 5 :step -1))
  (format t "~A~%" (z/db:recent-update-json i)))
;; add a project to the ignore-list (no updates will be generated)
(z/filter:add-to-ignore-list "<first-5-chars-of-a-project-id>")
;; you also have direct access to the rpc-api of cl-zenon; e.g.:
(z/api::with-node-at node "<your-node-ws-address>"
  (stats:os-info node))
;; or any of the other api methods. Note the ::, as this method is
;; normally not meant to be used - it locks a mutex.
```

Please report any issues or feedback here or ping @dumeril in the zenon chats. Thanks!

## Todo
  - Reorganize the code; especially move all `jsown`-methods to a dedicated package.
  - Publish updates as multipart-messages with the `type`-field as first message.
  - Check and cleanup unimplemented but exported symbols.
  - Implement the `copy-updates` method in `projects-db.lisp`, then implement the `updates-since` endpoint.
  - Implement more complete error checks and handlers to be able to serve unknown clients.

