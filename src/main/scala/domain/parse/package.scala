package domain

import domain.config.{ImageResolution, RenderSettings}
import domain.maths._
import domain.model._
import domain.utils.{AppError, InvalidArgumentError, ParseError}

import scala.annotation.tailrec

package object parse {

  type ErrorOr[V] = Either[AppError, V]

  private val lightSourceDeclarationRegex = """^(lightSource)\s*$""".r
  private val renderSettingsDeclarationRegex = """^(renderSettings)\s*$""".r
  private val imageResolutionDeclarationRegex = """^(imageResolution)\s*$""".r
  private val triangleDeclarationRegex = """^(triangle)\s*$""".r
  private val discDeclarationRegex = """^(disc)\s*$""".r
  private val ellipseDeclarationRegex = """^(ellipse)\s*$""".r
  private val sphereDeclarationRegex = """^(sphere)\s*$""".r
  private val spheroidDeclarationRegex = """^(spheroid)\s*$""".r
  private val ellipsoidDeclarationRegex = """^(ellipsoid)\s*$""".r
  private val cylinderDeclarationRegex = """^(cylinder)\s*$""".r
  private val coneDeclarationRegex = """^(cone)\s*$""".r

  private val colourRegex = """^([1-9]\d{0,2})\s+([1-9]\d{0,2})\s+([1-9]\d{0,2})$""".r
  private val doubleRegex = """^\-?\d+(\.\d+)?$""".r
  private val positiveDoubleRegex = """^\d+(\.\d+)?$""".r
  private val positiveIntegerRegex = """^[1-9]\d*$""".r
  private val rgbHexRegex = """^([0-9]|[abcdef])([0-9]|[abcdef])([0-9]|[abcdef])([0-9]|[abcdef])([0-9]|[abcdef])([0-9]|[abcdef])\s*$""".r

  def parseFileContents(lines: List[String]): ErrorOr[FileContents] = {
    val nonEmptyIndexedLines = lines.zipWithIndex.filter(_._1.trim.nonEmpty)
    val blocks = partitionIntoDeclaredBlocks(nonEmptyIndexedLines, List())
    processBlocks(blocks) flatMap { fileContentsAcc =>
      fileContentsAcc.toFileContents
    }
  }

  @tailrec
  private def partitionIntoDeclaredBlocks(
    indexedLines: List[(String, Int)],
    acc: List[List[(String, Int)]]
  ): List[List[(String, Int)]] =
    breakOffNextCandidateDeclaration(indexedLines) match {
      case (Nil, Nil) => acc
      case (nextBlock, Nil) => nextBlock :: acc
      case (nextBlock, remaining) => partitionIntoDeclaredBlocks(remaining, nextBlock :: acc)
    }

  private def breakOffNextCandidateDeclaration(
    indexedLines: List[(String, Int)]
  ): (List[(String, Int)], List[(String, Int)]) =
    indexedLines.tail.find(nonIndentedLine) match {
      case Some((_, index)) => indexedLines.partition(_._2 < index)
      case None => (indexedLines, List())
    }

  private def nonIndentedLine(line: (String, Int)): Boolean = line._1.trim.nonEmpty && !line._1.charAt(0).isWhitespace

  private def processBlocks(blocks: List[List[(String, Int)]]): ErrorOr[FileContentsAcc] = {
    val zero: ErrorOr[FileContentsAcc] = Right(FileContentsAcc.empty)
    blocks.foldLeft(zero) {
      case (Left(error), _) =>
        Left(error)
      case (Right(fileAcc), block) =>
        block match {
          case expectedDeclaration :: body =>
            expectedDeclaration._1 match {
              case lightSourceDeclarationRegex(_) =>
                withUniqueBlock(parseLightSource(body), fileAcc.lightSourceMaybe) { lightSource =>
                  Right(fileAcc.copy(lightSourceMaybe = Some(lightSource)))
                }
              case renderSettingsDeclarationRegex(_) =>
                withUniqueBlock(parseRenderSettings(body), fileAcc.renderSettingsMaybe) { renderSettings =>
                  Right(fileAcc.copy(renderSettingsMaybe = Some(renderSettings)))
                }
              case imageResolutionDeclarationRegex(_) =>
                withUniqueBlock(parseImageResolution(body), fileAcc.imageResolutionMaybe) { imageResolution =>
                  Right(fileAcc.copy(imageResolutionMaybe = Some(imageResolution)))
                }
              case triangleDeclarationRegex(_) =>
                parseTriangle(body).map(triangle => fileAcc.copy(objects = fileAcc.objects + triangle))
              case discDeclarationRegex(_) =>
                parseDisc(body).map(disc => fileAcc.copy(objects = fileAcc.objects + disc))
              case ellipseDeclarationRegex(_) =>
                parseEllipse(body).map(ellipse => fileAcc.copy(objects = fileAcc.objects + ellipse))
              case sphereDeclarationRegex(_) =>
                parseSphere(body).map(sphere => fileAcc.copy(objects = fileAcc.objects + sphere))
              case spheroidDeclarationRegex(_) =>
                parseSpheroid(body).map(spheroid => fileAcc.copy(objects = fileAcc.objects + spheroid))
              case ellipsoidDeclarationRegex(_) =>
                parseEllipsoid(body).map(ellipsoid => fileAcc.copy(objects = fileAcc.objects + ellipsoid))
              case cylinderDeclarationRegex(_) =>
                parseCylinder(body).map(cylinder => fileAcc.copy(objects = fileAcc.objects + cylinder))
              case coneDeclarationRegex(_) =>
                parseCone(body).map(cone => fileAcc.copy(objects = fileAcc.objects + cone))
              case _ => Left(ParseError(expectedDeclaration._2, s"Unable to parse ${expectedDeclaration._1}"))
            }
          case Nil =>
            Right(fileAcc)
        }
    }
  }

  private def withUniqueBlock[A](
    parsedOrError: ErrorOr[A],
    existingMaybe: Option[A]
  )(
    f: A => ErrorOr[FileContentsAcc]
  ): ErrorOr[FileContentsAcc] =
    (parsedOrError, existingMaybe) match {
      case (Right(parsed), None) => f(parsed)
      case (Right(parsed), Some(_)) => Left(ParseError(s"$parsed rejected - another instance in the same file"))
      case (Left(error), _) => Left(error)
    }

  private def parseRenderSettings(indexedLines: List[(String, Int)]): ErrorOr[RenderSettings] =
    indexedLines.sortBy(_._1) match {
      case maxReflections :: maxShadowRays :: Nil =>
        for {
          maxReflections <- parsePositiveIntegerField("maxReflections", maxReflections._1, maxReflections._2)
          maxShadowRaysPerPoint <- parsePositiveIntegerField("maxShadowRaysPerPoint", maxShadowRays._1, maxShadowRays._2)
          config <- RenderSettings(maxReflections, maxShadowRaysPerPoint)
        } yield config
      case defaultColour :: maxReflections :: maxShadowRays :: Nil =>
        for {
          maxReflections <- parsePositiveIntegerField("maxReflections", maxReflections._1, maxReflections._2)
          maxShadowRaysPerPoint <- parsePositiveIntegerField("maxShadowRaysPerPoint", maxShadowRays._1, maxShadowRays._2)
          defaultColour <- parseColourField("defaultColour", defaultColour._1, defaultColour._2)
          config <- RenderSettings(maxReflections, maxShadowRaysPerPoint, defaultColour = defaultColour)
        } yield config
      case antiAliasing :: defaultColour :: maxReflections :: maxShadowRays :: Nil =>
        for {
          maxReflections <- parsePositiveIntegerField("maxReflections", maxReflections._1, maxReflections._2)
          maxShadowRaysPerPoint <- parsePositiveIntegerField("maxShadowRaysPerPoint", maxShadowRays._1, maxShadowRays._2)
          antiAliasing <- parseBooleanField("antiAliasing", antiAliasing._1, antiAliasing._2)
          defaultColour <- parseColourField("defaultColour", defaultColour._1, defaultColour._2)
          config <- RenderSettings(maxReflections, maxShadowRaysPerPoint, antiAliasing, defaultColour)
        } yield config
      case _ => Left(ParseError("Wrong number of fields in 'renderSettings', expected 2 or 3"))
    }

  private def parseLightSource(indexedLines: List[(String, Int)]): ErrorOr[LightSource] =
    indexedLines.sortBy(_._1) match {
      case ambience :: location :: radius :: Nil =>
        for {
          amb <- parseProportionField("ambience", ambience._1, ambience._2)
          loc <- parseVectorField("location", location._1, location._2)
          rad <- parsePositiveNumberField("radius", radius._1, radius._2)
        } yield LightSource(loc, rad, amb)
      case _ => Left(ParseError("Wrong number of fields in 'lightSource': expected 2"))
    }


  private def parseImageResolution(indexedLines: List[(String, Int)]): ErrorOr[ImageResolution] =
    indexedLines.sortBy(_._1) match {
      case height :: width :: Nil =>
        for {
          w <- parsePositiveIntegerField("widthResolution", width._1, width._2)
          h <- parsePositiveIntegerField("heightResolution", height._1, height._2)
        } yield
          ImageResolution(
            width = w,
            height = h
          )
      case _ =>
        Left(ParseError("Wrong number of fields in 'imageSettings', expected 3"))
    }

  private def parseTriangle(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case colour :: reflectivity :: transparency :: vertex0 :: vertex1 :: vertex2 :: Nil =>
        for {
          v0 <- parseVectorField("vertex0", vertex0._1, vertex0._2)
          v1 <- parseVectorField("vertex1", vertex1._1, vertex1._2)
          v2 <- parseVectorField("vertex2", vertex2._1, vertex2._2)
          r <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          c <- parseColourField("colour", colour._1, colour._2)
          triangle <- Triangle(v0, v1, v2, r, t, c)
        } yield triangle
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"Expected 6 parameters for a triangle, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 6 parameters for a triangle"))
        )
    }

  private def parseDisc(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case centre :: colour :: horizontalTurn :: radius :: reflectivity :: transparency :: verticalTil :: Nil =>
        for {
          cen <- parseVectorField("centre", centre._1, centre._2)
          rad <- parsePositiveNumberField("radius", radius._1, radius._2)
          hor <- parseRadiansField("horizontalTurn", horizontalTurn._1, horizontalTurn._2)
          ver <- parseRadiansField("verticalTilt", verticalTil._1, verticalTil._2)
          refl <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          col <- parseColourField("colour", colour._1, colour._2)
        } yield Disc(
          centre = cen,
          radius = rad,
          horizontalTurn = hor,
          verticalTilt = ver,
          reflectivity = refl,
          transparency = t,
          colour = col
        )
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"Expected 7 parameters for a disc, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 7 parameters for a disc"))
        )
    }

  private def parseEllipse(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case axis0Length :: axis1Length :: centre :: colour :: horizontalTurn :: reflectivity :: transparency :: verticalTilt :: Nil =>
        for {
          cen <- parseVectorField("centre", centre._1, centre._2)
          ax0 <- parsePositiveNumberField("axis0Length", axis0Length._1, axis0Length._2)
          ax1 <- parsePositiveNumberField("axis1Length", axis1Length._1, axis1Length._2)
          hor <- parseRadiansField("horizontalTurn", horizontalTurn._1, horizontalTurn._2)
          ver <- parseRadiansField("verticalTilt", verticalTilt._1, verticalTilt._2)
          refl <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          col <- parseColourField("colour", colour._1, colour._2)
        } yield Ellipse(
          centre = cen,
          axis0Length = ax0,
          axis1Length = ax1,
          horizontalTurn = hor,
          verticalTilt = ver,
          reflectivity = refl,
          transparency = t,
          colour = col
        )
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"Expected 8 parameters for an ellipse, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 8 parameters for an ellipse"))
        )
    }

  private def parseSphere(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case centre :: colour :: radius :: reflectivity :: refractiveIndex :: transparency :: Nil =>
        for {
          cen <- parseVectorField("centre", centre._1, centre._2)
          rad <- parsePositiveNumberField("radius", radius._1, radius._2)
          refl <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          refr <- parseRefractiveIndexField("refractiveIndex", refractiveIndex._1, refractiveIndex._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          col <- parseColourField("colour", colour._1, colour._2)
        } yield Sphere(
          centre = cen,
          radius = rad,
          reflectivity = refl,
          refractiveIndex = refr,
          transparency = t,
          colour = col
        )
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"Expected 6 parameters for a sphere, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 6 parameters for a sphere"))
        )
    }

  private def parseSpheroid(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case axis0Length :: axis1Length :: centre :: colour :: horizontalTurn :: reflectivity :: refractiveIndex :: transparency :: verticalTilt :: Nil =>
        for {
          cen <- parseVectorField("centre", centre._1, centre._2)
          ax0 <- parsePositiveNumberField("axis0Length", axis0Length._1, axis0Length._2)
          ax1 <- parsePositiveNumberField("axis1Length", axis1Length._1, axis1Length._2)
          hor <- parseRadiansField("horizontalTurn", horizontalTurn._1, horizontalTurn._2)
          ver <- parseRadiansField("verticalTilt", verticalTilt._1, verticalTilt._2)
          refl <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          refr <- parseRefractiveIndexField("refractiveIndex", refractiveIndex._1, refractiveIndex._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          col <- parseColourField("colour", colour._1, colour._2)
        } yield Spheroid(
          centre = cen,
          axis0Length = ax0,
          axis1Length = ax1,
          horizontalTurn = hor,
          verticalTilt = ver,
          reflectivity = refl,
          refractiveIndex = refr,
          transparency = t,
          colour = col
        )
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"Expected 9 parameters for a spheroid, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 9 parameters for a spheroid"))
        )
    }

  private def parseEllipsoid(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case axis0Length :: axis1Length :: axis2Length :: centre :: colour :: horizontalTurn :: reflectivity :: refractiveIndex :: transparency :: verticalTilt :: Nil =>
        for {
          cen <- parseVectorField("centre", centre._1, centre._2)
          ax0 <- parsePositiveNumberField("axis0Length", axis0Length._1, axis0Length._2)
          ax1 <- parsePositiveNumberField("axis1Length", axis1Length._1, axis1Length._2)
          ax2 <- parsePositiveNumberField("axis2Length", axis2Length._1, axis2Length._2)
          hor <- parseRadiansField("horizontalTurn", horizontalTurn._1, horizontalTurn._2)
          ver <- parseRadiansField("verticalTilt", verticalTilt._1, verticalTilt._2)
          refl <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          refr <- parseRefractiveIndexField("refractiveIndex", refractiveIndex._1, refractiveIndex._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          col <- parseColourField("colour", colour._1, colour._2)
        } yield Ellipsoid(
          centre = cen,
          axis0Length = ax0,
          axis1Length = ax1,
          axis2Length = ax2,
          horizontalTurn = hor,
          verticalTilt = ver,
          reflectivity = refl,
          refractiveIndex = refr,
          transparency = t,
          colour = col
        )
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"Expected 10 parameters for an ellipsoid, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 10 parameters for an ellipsoid"))
        )
    }

  private def parseCylinder(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case centre :: colour :: horizontal :: length :: radius :: reflectivity :: refractiveIndex :: transparency :: typeOfCylinder :: verticalTilt :: Nil =>
        for {
          cen <- parseVectorField("centre", centre._1, centre._2)
          rad <- parsePositiveNumberField("radius", radius._1, radius._2)
          len <- parsePositiveNumberField("length", length._1, length._2)
          typ <- parseCylinderTypeField("type", typeOfCylinder._1, typeOfCylinder._2)
          hor <- parseRadiansField("horizontalTurn", horizontal._1, horizontal._2)
          ver <- parseRadiansField("verticalTilt", verticalTilt._1, verticalTilt._2)
          refl <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          refr <- parseRefractiveIndexField("refractiveIndex", refractiveIndex._1, refractiveIndex._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          col <- parseColourField("colour", colour._1, colour._2)
        } yield Cylinder(
          centre = cen,
          radius = rad,
          length = len,
          cylinderType = typ,
          horizontalTurn = hor,
          verticalTilt = ver,
          reflectivity = refl,
          refractiveIndex = refr,
          transparency = t,
          colour = col
        )
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"expected 10 parameters for a cylinder, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 10 parameters for a cylinder"))
        )
    }

  private def parseCone(lines: List[(String, Int)]): ErrorOr[Shape] =
    lines.sortBy(_._1) match {
      case apex :: colour :: endRadius :: height :: horizontalTurn :: reflectivity :: refractiveIndex :: transparency :: typeOfCone :: verticalTilt :: Nil =>
        for {
          ape <- parseVectorField("apex", apex._1, apex._2)
          rad <- parsePositiveNumberField("endRadius", endRadius._1, endRadius._2)
          hei <- parsePositiveNumberField("height", height._1, height._2)
          typ <- parseConeTypeField("type", typeOfCone._1, typeOfCone._2)
          hor <- parseRadiansField("horizontalTurn", horizontalTurn._1, horizontalTurn._2)
          ver <- parseRadiansField("verticalTilt", verticalTilt._1, verticalTilt._2)
          refl <- parseProportionField("reflectivity", reflectivity._1, reflectivity._2)
          refr <- parseRefractiveIndexField("refractiveIndex", refractiveIndex._1, refractiveIndex._2)
          t <- parseProportionField("transparency", transparency._1, transparency._2)
          col <- parseColourField("colour", colour._1, colour._2)
        } yield Cone(
          apex = ape,
          endRadius = rad,
          height = hei,
          coneType = typ,
          horizontalTurn = hor,
          verticalTilt = ver,
          reflectivity = refl,
          refractiveIndex = refr,
          transparency = t,
          colour = col
        )
      case _ =>
        Left(
          lines.headOption.map(headWithIndex =>
            ParseError(headWithIndex._2 + 1, s"Expected 10 parameters for a cone, found ${lines.map(_._1)}")
          ).getOrElse(ParseError("Expected 10 parameters for a cone"))
        )
    }

  private def parseRefractiveIndexField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[RefractiveIndex] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { refractiveIndexString =>
      positiveDoubleRegex.findFirstIn(refractiveIndexString) match {
        case Some(positiveNumber) => RefractiveIndex(positiveNumber.toDouble)
        case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $refractiveIndexString as a refractive index"))
      }
    }

  private def parseBooleanField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[Boolean] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { booleanString =>
      booleanString.trim match {
        case "true" => Right(true)
        case "false" => Right(false)
        case _ => Left(ParseError(fieldIndex + 1, s"Unable to read '$booleanString' as a true/false boolean value"))
      }
    }

  private def parseColourField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[Colour] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { colourString =>
      colourString match {
        case rgbHexRegex(r0, r1, g0, g1, b0, b1) =>
          Colour(
            redValue = Integer.parseInt(r0 + r1, 16),
            greenValue = Integer.parseInt(g0 + g1, 16),
            blueValue = Integer.parseInt(b0 + b1, 16)
          )
        case _ =>
          Left(ParseError(fieldIndex + 1, s"Unable to interpret $colourString as a colour - must be a hexadecimal value of length 6"))
      }
    }

  private def parsePositiveNumberField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[PositiveNumber] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { positiveString =>
      positiveDoubleRegex.findFirstIn(positiveString) match {
        case Some(positiveNumberString) => PositiveNumber(positiveNumberString.toDouble)
        case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $positiveString as a positive number"))
      }
    }

  private def parsePositiveIntegerField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[PositiveInteger] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { positiveString =>
      positiveIntegerRegex.findFirstIn(positiveString) match {
        case Some(positiveIntegerString) => PositiveInteger(positiveIntegerString.toInt)
        case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $positiveString as a positive integer"))
      }
    }

  private def parseProportionField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[Proportion] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { proportionString =>
      doubleRegex.findFirstIn(proportionString) match {
        case Some(proportion) => Proportion(proportion.toDouble)
        case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $proportionString as a real number"))
      }
    }

  private def parseRadiansField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[Radians] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { radianString =>
      doubleRegex.findFirstIn(radianString).map(radians => Radians(radians.toDouble)) match {
        case Some(radians) => Right(radians)
        case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $radianString as an angle in radians"))
      }
    }

  private def parseConeTypeField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[ConeType] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { typeString =>
      ConeType.fromString(typeString) match {
        case Some(coneType) => Right(coneType)
        case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $typeString as a cone type from ${ConeType.recognisedInputs}"))
      }
    }

  private def parseCylinderTypeField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[CylinderType] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { typeString =>
      CylinderType.fromString(typeString) match {
        case Some(cylinderType) => Right(cylinderType)
        case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $typeString as a cylinder type from ${CylinderType.recognisedInputs}"))
      }
    }

  private def parseVectorField(expectedLabel: String, field: String, fieldIndex: Int): ErrorOr[CartesianVector] =
    parseLabelledValue(expectedLabel, field, fieldIndex) { vectorString =>
      if (vectorString.startsWith("(") && vectorString.endsWith(")")) {
        val coordinates = vectorString.substring(1, vectorString.length - 1)
        coordinates.split(",").toList.map(_.trim) match {
          case xString :: yString :: zString :: Nil =>
            val vectorMaybe = for {
              x <- doubleRegex.findFirstIn(xString).map(_.toString.toDouble)
              y <- doubleRegex.findFirstIn(yString).map(_.toString.toDouble)
              z <- doubleRegex.findFirstIn(zString).map(_.toString.toDouble)
            } yield CartesianVector(x, y, z)
            vectorMaybe match {
              case Some(vector) => Right(vector)
              case None => Left(ParseError(fieldIndex + 1, s"Unable to interpret $vectorString as a vector with Cartesian coordinates"))
            }
          case _ =>
            Left(ParseError(fieldIndex + 1, s"Unable to interpret $vectorString as a vector with three coordinates"))
        }
      } else {
        Left(ParseError(fieldIndex + 1, s"Unable to interpret $vectorString as a vector '(x, y, z)'"))
      }
    }

  private def parseLabelledValue[V](expectedLabel: String, argument: String, index: Int)(parseValue: String => ErrorOr[V]): ErrorOr[V] =
    argument.split(":").toList.map(_.trim) match {
      case label :: valueString :: Nil if label == expectedLabel => parseValue(valueString)
      case label :: valueString :: Nil => Left(ParseError(index + 1, s"expected '$expectedLabel' field, found '$label' field with value '$valueString'"))
      case _ => Left(ParseError(index + 1, s"Unable to parse $argument"))
    }

  private final case class FileContentsAcc(
    renderSettingsMaybe: Option[RenderSettings],
    imageResolutionMaybe: Option[ImageResolution],
    lightSourceMaybe: Option[LightSource],
    objects: Set[Shape] = Set()
  ) {
    def toFileContents: ErrorOr[FileContents] =
      (renderSettingsMaybe, lightSourceMaybe, imageResolutionMaybe) match {
        case (Some(renderSettings), Some(lightSource), Some(imageSettings)) =>
          Right(FileContents(renderSettings, imageSettings, lightSource, objects))
        case _ =>
          Left(InvalidArgumentError(s"Unable to create FileContents: missing fields " + missingMandatoryFields.mkString(",")))
      }

    private def missingMandatoryFields: List[String] =
      renderSettingsMaybe.map(_ => List()).getOrElse(List("renderSettings")) ++
        imageResolutionMaybe.map(_ => List()).getOrElse(List("imageResolution")) ++
        lightSourceMaybe.map(_ => List()).getOrElse(List("lightSource"))
  }

  private object FileContentsAcc {
    def empty: FileContentsAcc =
      FileContentsAcc(
        renderSettingsMaybe = None,
        imageResolutionMaybe = None,
        lightSourceMaybe = None,
        objects = Set()
      )
  }
}
