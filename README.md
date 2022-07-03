# zenon-az-service

A service providing information and updates about Zenon Accelerator-Z proposals and updates.

This service is the data backend for the telegram channel https://t.me/acceleratorz_updates. It does not contain the bot implementation, for that, see https://github.com/dumeriz/zaz-telegram-py, which is using this service.

Two ZMQ endpoints are provided:
  - A REQ socket, to answer queries about proposals
  - A PUB socket, publishing updates on proposals and regular statistics on Pillar participation.

State is managed in an in-memory db, which is simply a hash-table.  As there is no subscription api for the accelerator contract in znnd, 
the service polls the `getAll`-endpoint periodically and generates a diff with the last known state. Changes are persisted and published as json data.  Periodically, statistics about pillar participation rates are generated and also published.

The service writes to two locations on disk:
  - `~/.local/share/zaz`: A backup file `backup.cls` is written here before each update
  - `~/.cache/zaz/log`: For rolling logfile.

Those locations are configurable, see parameters in `projects-db.lisp`.

## Usage
This service is the data backend for the accelerator-z telegram channel. The endpoints are not public, and probably won't be, because I don't want to have to deal with rogue clients. However, if you want to access them, e.g. to implement bots for other platfroms, let me know and I can set up an instance for you. You can also self host:

### Self hosting
To run the service, use sbcl on Linux and install the quicklisp library.
Then, link this code into ql's `local-projects`. Get the Zenon-API-SDK for Common Lisp from https://github.com/dumeriz/cl-zenon.
Register that also as local project.
In a REPL:
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

## TODO
  - Reorganize the code; especially move all `jsown`-methods to a dedicated package.
  - Publish updates as multipart-messages with the `type`-field as first message.
  - Check and cleanup unimplemented but exported symbols.
  - Implement the `copy-updates` method in `projects-db.lisp`, then implement the `updates-since` endpoint.
  - Implement more complete error checks and handlers to be able to serve unknown clients.

