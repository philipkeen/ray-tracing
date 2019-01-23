package domain

import domain.config.ImageResolution
import domain.error.AppError
import domain.model.{Colour, Pixel}
import domain.utils.NonEmptyString
import task.Task

trait Picture {

  def imageName: NonEmptyString

  def imageResolution: ImageResolution

  def paintPixel(pixel: Pixel, colour: Colour): Task[Either[AppError, Unit]]

  def persist: Task[Unit]
}
