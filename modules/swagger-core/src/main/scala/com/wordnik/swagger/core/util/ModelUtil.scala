/**
 *  Copyright 2013 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.core.util

import com.wordnik.swagger.model._
import com.wordnik.swagger.converter.ModelConverters
import com.wordnik.swagger.core.{ SwaggerContext, SwaggerSpec, SwaggerTypes }

import org.slf4j.LoggerFactory

import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }

object ModelUtil {
  private val LOGGER = LoggerFactory.getLogger(ModelUtil.getClass)
  val ComplexTypeMatcher =  "([\\w]*)\\[([\\w\\.\\-]*)\\].*".r
  val CustomTypeMatcher = """([\w\.\-]*)\[([\w\.\-]*)\].*""".r


  def stripPackages(apis: List[ApiDescription]): List[ApiDescription] = {
    (for(api <- apis) yield {
      val operations = (for(op <- api.operations) yield {
        val parameters = (for(param <- op.parameters) yield {
          param.copy(dataType = cleanDataType(param.dataType))
        }).toList
        val messages = (for(message <- op.responseMessages) yield {
          if(message.responseModel != None) {
            message.copy(responseModel = Some(cleanDataType(message.responseModel.get)))
          }
          else message
        }).toList
        op.copy(
          responseClass = cleanDataType(op.responseClass),
          parameters = parameters,
          responseMessages = messages)
      }).toList
      api.copy(operations = operations)
    }).toList
  }

  def cleanDataType(dataType: String): String = {
    val typeInfo = dataType match {
      case ComplexTypeMatcher(container, inner) => {
        val p = if(inner.indexOf(",") > 0)
          inner.split("\\,").last.trim
        else inner
        (Some(container), p)
      }
      case CustomTypeMatcher(outer, inner) => {
        val p = if (outer.indexOf('.') > 0)
          outer.split('.').last.trim
        else outer
        (Some(p), inner)
      }
      case _ => (None, dataType)
    }
    val baseType = if(typeInfo._2.startsWith("java.lang")) {
      val trimmed = typeInfo._2.substring("java.lang".length + 1)
      if(SwaggerSpec.baseTypes.contains(trimmed.toLowerCase))
        trimmed.toLowerCase
      else
        trimmed
    }
    else {
      modelFromString(typeInfo._2) match {
        case Some(e) => e._1
        case None => typeInfo._2
      }
    }
    val normalized = SwaggerTypes(baseType) match {
      case "object" => baseType
      case e: String => e
    }
    // put back in container
    typeInfo._1 match {
      case Some(e) => "%s[%s]".format(e, normalized)
      case None => normalized
    }
  }

  def modelsFromApis(apis: List[ApiDescription]): Option[Map[String, Model]] = {
    val modelnames = new HashSet[String]()
    for(api <- apis; op <- api.operations) {
      modelnames ++= op.responseMessages.map{_.responseModel}.flatten.toSet
      modelnames += op.responseClass
      op.parameters.foreach(param => {
        LOGGER.debug("adding dependent model " + param.dataType)
        modelnames += param.dataType
      })
    }
    val models = (for(name <- modelnames) yield modelAndDependencies(name)).flatten.toMap
    if(models.size > 0) Some(models)
    else None
  }

  def modelAndDependencies(name: String): Map[String, Model] = {
    // typeRefs is a list of tuples (clsName, Option[modelName])
    // For a custom container foo.bar.MyContainer[MyContainee], the
    // clsName = foo.bar.MyContainer
    // modelName = foo.bar.MyContainer[MyContainee]
    // The package names are stripped later.
    // Otherwise, the modeName is the same as the clsName.
    val typeRefs: List[(String, Option[String])] = name match {
      case CustomTypeMatcher(containerType, basePart) => {
        if (SwaggerSpec.containerTypes.contains(containerType)) {
            LOGGER.debug("loading " + basePart + ", " + containerType)
            List(if (basePart.indexOf(",") > 0) // handle maps, i.e. List[String,String]
              (basePart.split("\\,").last.trim, None)
            else (basePart, None))
          } else {
            // If the containerType is not one of the SwaggerSpec supported types, then
            // it is a generic container type that needs to be supported as a Model
            // itself along with the containee basePart
            val customContainer = s"${containerType}[${basePart}]"
            List((containerType, Some(customContainer)), (basePart, None))
          }
        }
      case _ => List((name, None))
    }

    val listOfLists = (for (typeRef <- typeRefs) yield ({
      if (shouldIncludeModel(typeRef._1)) {
        try {
          val cls = SwaggerContext.loadClass(typeRef._1)
          val modelTupleList = (for (model <- ModelConverters.readAll(cls, typeRef._2)) yield (model.name, model))
          modelTupleList
        }
        catch {
          case e: ClassNotFoundException => List()
        }
      }
      else List()
    }))
    val flatList = listOfLists.flatten
    val modelMap = flatList.toMap
    modelMap
  }

  def modelFromString(name: String): Option[Tuple2[String, Model]] = {
    val typeRef = name match {
      case ComplexTypeMatcher(containerType, basePart) => {
        if(basePart.indexOf(",") > 0) // handle maps, i.e. List[String,String]
          basePart.split("\\,").last.trim
        else basePart
      }
      case _ => name
    }
    if(shouldIncludeModel(typeRef)) {
      try{
        val cls = SwaggerContext.loadClass(typeRef)
        if (!cls.isEnum) {
          ModelConverters.read(cls, ModelConverters.typeMap) match {
            case Some(model) => Some((toName(cls), model))
            case None => None
          }
        }
        else None
      }
      catch {
        case e: ClassNotFoundException => None
      }
    }
    else None
  }

  /**
   * Given a Java Class, return the name to use for the Model name and id.
   * The class name may include the fully-qualified package while the Model name
   * will be the simple name.
   * @param cls
   * @return - name to use as the model name
   */
  def toName(cls: Class[_]): String = {
    import javax.xml.bind.annotation._

    val xmlRootElement = cls.getAnnotation(classOf[XmlRootElement])
    val xmlEnum = cls.getAnnotation(classOf[XmlEnum])

    if (xmlEnum != null && xmlEnum.value != null)
      toName(xmlEnum.value())
    else if (xmlRootElement != null) {
      if ("##default".equals(xmlRootElement.name())) {
        cls.getSimpleName 
      } else {
        xmlRootElement.name() 
      }
    } else if (cls.getName.startsWith("java.lang.")) {
      val name = cls.getName.substring("java.lang.".length)
      val lc = name.toLowerCase
      if(SwaggerSpec.baseTypes.contains(lc)) lc
      else name
    }
    else if (cls.getName.indexOf(".") < 0) cls.getName
    else cls.getSimpleName 
  }

  def shouldIncludeModel(modelname: String) = {
    if(SwaggerSpec.baseTypes.contains(modelname.toLowerCase))
      false
    else if(modelname.startsWith("java.lang"))
      false
    else true
  }

  /**
   * Convert a java.lang primitive class name or type to the corresponding
   * SwaggerSpec base type.
   * @param name
   * @return Swagger base type name if applicable or no-op returning name
   *         if not a base type
   */
  def javaPrimitiveToSwaggerBaseTypeName(name: String): String = {
    if (name.startsWith("java.lang.")) {
      val sName = name.substring("java.lang.".length)
      val lc = sName.toLowerCase
      if (SwaggerSpec.baseTypes.contains(lc)) lc else name
    } else name
  }

  /**
   * Convert a fully-qualified class name to the simple name. If name
   * is already simple, the name is simply returned.
   * @param name
   * @return simple name
   */
  def stripPackage(name: String): String ={
    val n1 = if (name.indexOf('.') > 0) name.split('.').last.trim else name
    n1
  }

  /**
   * If the name contains multiple classes separated by a comma, the
   * final class is returned.  If no comma exists, the input name is returned
   * @param name
   * @return
   */
  def stripMapName(name: String): String ={
    val n1 = if (name.indexOf(',') > 0) name.split(',').last.trim else name
    n1
  }

  /**
   * Split a class name or fully-qualified class name into the
   * container class and the containee class(es).
   * ex: foo.bar.MyList[foo.bar.Yadda,foo.bar.Lala] will be
   * (Some(foo.bar.MyList), Some(foo.bar.Yadda,foo.bar.Lala))
   * @param name
   * @return Tuple of container and containee
   */
  def splitContainer(name: String): (Option[String], Option[String]) ={
    name match {
      case CustomTypeMatcher(outer, inner) => (Some(outer), Some(inner))
      case inner => (None, Some(inner))
    }
  }

  /**
   * Convert a fully qualified model name (possibly including
   * 1 level of parameterization of a generic container class) to
   * the appropriate model name.
   * @param fullyQualifiedModelName
   * @return Option[modelName] if successful
   *         None if unsuccessful
   */
  def toName(fullyQualifiedModelName: String): Option[String] ={
    splitContainer(fullyQualifiedModelName) match {
      case (Some(outer), Some(inner)) => {
        val outerSimple = toName(outer).fold(outer)(o => o)
        val innerSimple = toName(inner).fold(inner)(i => i)
        Some(s"$outerSimple[$innerSimple]")
      }
      case (None, Some(inner)) => Some(stripPackage(stripMapName(javaPrimitiveToSwaggerBaseTypeName(inner))))
      case _ => None
    }
  }
}