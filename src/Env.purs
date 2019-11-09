module Conduit.Env where

import SQLite3 (DBConnection)

type Env = { db :: DBConnection, jwtSecret :: String }
