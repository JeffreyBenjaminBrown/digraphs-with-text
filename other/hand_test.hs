-- lens test
  import qualified Control.Lens as Ln
  
  type Degrees = Double
  type Latitude = Degrees
  type Longitude = Degrees
  
  data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) }
  Ln.makeLenses ''Meetup
  
  let m = Meetup { _name = "hi", _location = (1,2) }
  Ln.view location $ Ln.set location (1,3) m

