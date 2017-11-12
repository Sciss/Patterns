package de.sciss.patterns

import de.sciss.patterns.Types.Top

trait Event extends Top {
  type Out = Map[String, _]
}
