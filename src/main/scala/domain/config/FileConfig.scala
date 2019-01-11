package domain.config

import domain.utils.NonEmptyString

final case class FileConfig(
  directory: Directory,
  sceneFile: NonEmptyString,
  imageName: NonEmptyString
)

abstract case class Directory(value: String)

object Directory {
  def apply(value: String): Option[Directory] =
    if (value.endsWith("/")) {
      Some( new Directory(value) {})
    } else {
      None
    }
}
