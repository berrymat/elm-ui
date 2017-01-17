module Container.Element exposing (..)

import Roots.Root as Root
import Customers.Customer as Customer
import Clients.Client as Client
import Sites.Site as Site
import Staffs.Staff as Staff


type Element
    = RootEntity Root.Root
    | CustomerEntity Customer.Customer
    | ClientEntity Client.Client
    | SiteEntity Site.Site
    | StaffEntity Staff.Staff
