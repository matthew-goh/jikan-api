package models

object SortOrders extends Enumeration {
  val asc, desc, none = Value
}

object FavouritesOrders extends Enumeration {
  val title, start_year, none = Value
}

object SavedAnimeOrders extends Enumeration {
  val saved_at, title, year, score = Value
}

object SavedAnimeStatus extends Enumeration {
  val completed, watching, not_started, all = Value
}

object CharacterRoles extends Enumeration {
  val main, supporting, all = Value
}

object VoicedCharacterOrders extends Enumeration {
  val anime, character, none = Value
}
