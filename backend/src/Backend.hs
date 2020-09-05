module Backend where

import Common.Route (BackendRoute, FrontendRoute, fullRouteEncoder)
import Obelisk.Backend (Backend (Backend, _backend_run, _backend_routeEncoder))
import Game ()

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  {
    _backend_run = \serve -> serve $ const $ return (),
    _backend_routeEncoder = fullRouteEncoder
  }
