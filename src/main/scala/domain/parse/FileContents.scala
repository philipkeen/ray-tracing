package domain.parse

import domain.config.{ImageResolution, RenderSettings}
import domain.model.{LightSource, Shape}

final case class FileContents(
  renderSettings: RenderSettings,
  imageResolution: ImageResolution,
  lightSource: LightSource,
  objects: Set[Shape]
)
