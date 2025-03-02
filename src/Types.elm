module Types exposing (Post, PostDetails, Template, User)


type alias User =
    { id : Int
    , username : String
    }


type alias Template =
    { id : Int
    , name : String
    }


type alias Post =
    { id : Int
    , subject : String
    }


type alias PostDetails =
    { id : Int
    , authors : List User
    }
