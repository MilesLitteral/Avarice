module Avarice.Generic.Prisms where 

    import Data.Monoid ( First )
    import Control.Lens.Combinators ( preview, review, Getting, AReview )

    (##) :: Getting (First a) s a -> s -> Maybe a
    x ## y   = preview x y

    (#~) :: b -> AReview t b -> t
    xl #~ yl = review yl xl