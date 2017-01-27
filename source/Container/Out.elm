module Container.Out exposing (..)

import Roots.Root exposing (..)
import Customers.Customer exposing (..)
import Clients.Client exposing (..)
import Sites.Site exposing (..)
import Staffs.Staff exposing (..)
import Helpers.Models exposing (..)


type OutMsg
    = OutCancel
    | OutUpdateRoot HttpMethod Root
    | OutDeleteRoot Root
    | OutUpdateCustomer HttpMethod Customer
    | OutDeleteCustomer Customer
    | OutUpdateClient HttpMethod Client
    | OutDeleteClient Client
    | OutUpdateSite HttpMethod Site
    | OutDeleteSite Site
    | OutUpdateStaff HttpMethod Staff
    | OutDeleteStaff Staff
    | OutNone
