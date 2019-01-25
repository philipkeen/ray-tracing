package domain

import domain.config.ImageResolution
import domain.error.AppError
import domain.model.{Colour, Pixel}
import domain.utils.NonEmptyString

trait Picture[F[_]] {

  def imageName: NonEmptyString

  def imageResolution: ImageResolution

  def paintPixel(pixel: Pixel, colour: Colour): F[Either[AppError, Unit]]

  def persist: F[Unit]
}
