module FFProbe.Data.Misc where

type TagList = [(String, TagValue)]

data TagValue
    = StringTag String
    | IntTag Integer
    | FloatTag Float
    | -- | If the constructor is 'Other', the String is a JSON representation of the value
      Other String