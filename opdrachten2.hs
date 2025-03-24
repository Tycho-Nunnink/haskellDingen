lucky 7 = "lucky bastard"
lucky _ = "sucker"

increment :: (Num a) => a -> a
increment = (+1)

third (_,_,x) = x

data Geometry = Rectangle Int Int | Triangle Int Int | Circle Int
