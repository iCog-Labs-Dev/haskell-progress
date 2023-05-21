module Employee where
import Test.HUnit
type Name = String

data Employee = Employee {
    name :: Name,
    phone :: String
}deriving(Show)

employee :: Name -> String -> Employee
employee name phone = Employee {name = name, phone = phone}

maybeEmployee :: Maybe Name -> Maybe String -> Maybe Employee
maybeEmployee name@(Just a) phone@(Just b) 
              | name == Nothing || phone == Nothing = Nothing
              | otherwise = Just (Employee {name = a, phone = b})


combinedEmployee :: (Name -> String -> Employee) -> (Maybe Name -> Maybe String -> Maybe Employee)
combinedEmployee emp maybeName maybePhone = emp <$> maybeName <*> maybePhone

test1 = combinedEmployee employee Nothing Nothing
test2 = combinedEmployee employee Nothing (Just "123-456-7890")
test3 = combinedEmployee employee (Just "John Doe") Nothing
test4 = combinedEmployee employee (Just "John Doe") (Just "123-456-7890")


