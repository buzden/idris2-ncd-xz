module Information.Distance.NCD

import Data.Contravariant
import public Data.Double.Bounded

%default total

public export
interface NCD m a | a where
  ncd : a -> a -> m $ DoubleBetween 0 1

export
Contravariant (NCD m) where
  contramap {a} f sub = CN where
    %inline [CN] NCD m a where ncd = ncd @{sub} `on` f

export %hint
FromId : NCD Prelude.id a => Applicative m => NCD m a
FromId @{sub} = AN where
  %inline [AN] NCD m a where ncd = pure .: ncd @{sub}

export %inline
ByShow : Show a => NCD m String => NCD m a
ByShow = show >$< %search

export %inline
ByInterpolate : Interpolation a => NCD m String => NCD m a
ByInterpolate = interpolate >$< %search
