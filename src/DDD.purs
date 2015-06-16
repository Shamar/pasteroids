module DDD where


class (Eq id) <= Entity e id where
  identify :: e -> id
