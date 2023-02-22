data User = Admin String  
          | Client String Int
          deriving Show

namedClient :: User
namedClient = Client "Client Name" 0
