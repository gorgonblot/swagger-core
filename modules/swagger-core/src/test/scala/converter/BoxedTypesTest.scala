package converter

import com.wordnik.swagger.core.SwaggerSpec
import com.wordnik.swagger.core.util.ModelUtil
import converter.models._
import com.wordnik.swagger.model._
import com.wordnik.swagger.converter._
import org.json4s._
import org.json4s.jackson.Serialization.write
import org.json4s.jackson._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

@RunWith(classOf[JUnitRunner])
class BoxedTypesTest extends FlatSpec with Matchers {
  implicit val formats = SwaggerSerializers.formats

 "ModelConverters" should "format a BoxedType" in {
    val model = ModelConverters.read(classOf[BoxedTypesIssue31]).getOrElse(fail("no model found"))
    model.properties.size should be (5)
    write(model) should be ("""{"id":"BoxedTypesIssue31","description":"Options of boxed types produces an Object ref instead of correct type","properties":{"stringSeq":{"type":"array","items":{"type":"string"}},"stringOpt":{"type":"string"},"intSeq":{"type":"array","description":"Integers in a Sequence Box","items":{"$ref":"Object"}},"intOpt":{"$ref":"Object","description":"Integer in an Option Box"},"justInt":{"type":"integer","format":"int32"}}}""")
  }

  it should "format a BoxedTypeWithDataType provided in the annotation for the boxed object types" in {
    val model = ModelConverters.read(classOf[BoxedTypesIssue31WithDataType]).getOrElse(fail("no model found"))
    model.properties.size should be (5)
    write(model) should be ("""{"id":"BoxedTypesIssue31WithDataType","description":"Options of boxed types produces an Object ref instead of correct type, but can be overcome with dataType","properties":{"stringSeq":{"type":"array","items":{"type":"string"}},"stringOpt":{"type":"string"},"intSeq":{"type":"array","description":"Integers in a Sequence Box","items":{"type":"integer","format":"int32"}},"intOpt":{"type":"integer","format":"int32","description":"Integer in an Option Box"},"justInt":{"type":"integer","format":"int32"}}}""")
  }

  it should "format a ListReply[T] parameterized with a BoxedTypeWithDataType" in {
    // Without special handling and basing the Spec only on the Java Class returned from
    // classOf, the Spec is unable to fill in the parameterized value for T in the generic class.
    // This tests the expected (but undesired) result based on the limitations of Java reflection resulting from erasure.
    val model = ModelConverters.read(classOf[ListReply[BoxedTypesIssue31WithDataType]]).getOrElse(fail("no model found"))
    model.properties.size should be (5)
    val modelStr = write(model)
    val jModel = Serialization.read[JValue](modelStr)
    modelStr should be("""{"id":"ListReply","description":"Standard reply containing a list of items and paging information","properties":{"items":{"type":"array","description":"List of requested items (in current page if paged)","items":{"$ref":"T"}},"total":{"type":"integer","format":"int32","description":"Total number of items in the reply (across all pages if applicable)"},"nextPageToken":{"type":"string","description":"Identifier used to fetch the next page of results"},"offset":{"type":"integer","format":"int32","description":"Offset within the total count of results where this current items list starts"},"limit":{"type":"integer","format":"int32","description":"Limit on the number of items included in a single response page"}}}""")
    val jId = jModel \ "id"
    jId.extract[String] should be ("ListReply")
  }

  "ModelUtil" should "create a model for the outer class of a ListReply from a Java Class name" in {
    val fqModelName = classOf[ListReply[BoxedTypesIssue31WithDataType]].getName()
    val modelNameOpt = ModelUtil.toName(fqModelName)
    modelNameOpt should be ('defined)
    val modelName = modelNameOpt.get
    val models: Map[String, Model] = ModelUtil.modelAndDependencies(fqModelName)

    val modelList = models.toList
    modelList.size should be (1)

    val modelOpt = models.get(modelName)
    modelOpt should be ('defined)
    val model = modelOpt.get
    model.properties.size should be (5)
    val modelStr = write(model)
    val jModel = Serialization.read[JValue](modelStr)
    modelStr should be("""{"id":"ListReply","description":"Standard reply containing a list of items and paging information","properties":{"items":{"type":"array","description":"List of requested items (in current page if paged)","items":{"$ref":"T"}},"total":{"type":"integer","format":"int32","description":"Total number of items in the reply (across all pages if applicable)"},"nextPageToken":{"type":"string","description":"Identifier used to fetch the next page of results"},"offset":{"type":"integer","format":"int32","description":"Offset within the total count of results where this current items list starts"},"limit":{"type":"integer","format":"int32","description":"Limit on the number of items included in a single response page"}}}""")
    val jId = jModel \ "id"
    jId.extract[String] should be ("ListReply")
  }

  it should "create a model for the inner class of a ListReply[T] from a fully qualified model name String" in {
    val fqModelName: String = "ListReply[converter.models.BoxedTypesIssue31WithDataType]"
    val modelNameOpt = ModelUtil.toName(fqModelName)
    modelNameOpt should be ('defined)
    val modelName = modelNameOpt.get
    val models: Map[String, Model] = ModelUtil.modelAndDependencies(fqModelName)
    val (outerOpt, innerOpt) = ModelUtil.splitContainer(modelName)
    outerOpt should be ('defined)
    innerOpt should be ('defined)
    val outerName = outerOpt.get
    val innerName = innerOpt.get
    // Without using a special ListReplyConverter, the generic ListReply container is not seen as a model
    // and only the inner type is in the model list.
    models.size should be (1)
    val modelOpt = models.get(innerName)
    modelOpt should be ('defined)
    val model = modelOpt.get
    model.properties.size should be (5)
    val modelStr = write(model)
    val jModel = Serialization.read[JValue](modelStr)
    modelStr should be("""{"id":"BoxedTypesIssue31WithDataType","description":"Options of boxed types produces an Object ref instead of correct type, but can be overcome with dataType","properties":{"stringSeq":{"type":"array","items":{"type":"string"}},"stringOpt":{"type":"string"},"intSeq":{"type":"array","description":"Integers in a Sequence Box","items":{"type":"integer","format":"int32"}},"intOpt":{"type":"integer","format":"int32","description":"Integer in an Option Box"},"justInt":{"type":"integer","format":"int32"}}}""")
    val jId = jModel \ "id"
    jId.extract[String] should be ("BoxedTypesIssue31WithDataType")
  }

  it should "format a ListReply[T] parameterized with a BoxedTypeWithDataType using the ListReplyConverter" in {
    val lrc = new ListReplyConverter
    ModelConverters.addConverter(lrc, true)

    val fqModelName = "converter.models.ListReply[converter.models.BoxedTypesIssue31WithDataType]"
    val modelNameOpt = ModelUtil.toName(fqModelName)
    modelNameOpt should be ('defined)
    val modelName = modelNameOpt.get
    modelName should equal ("ListReply[BoxedTypesIssue31WithDataType]")
    val (outerNameOpt, innerNameOpt) = ModelUtil.splitContainer(modelName)
    outerNameOpt should be ('defined)
    innerNameOpt should be ('defined)

    val innerName = innerNameOpt.get
    val outerName = outerNameOpt.get

    innerName should equal ("BoxedTypesIssue31WithDataType")
    outerName should equal ("ListReply")

    val models: Map[String, Model] = ModelUtil.modelAndDependencies(fqModelName)

    val modelList = models.toList
    modelList.size should be (2)

    val modelOpt = models.get(modelName)
    modelOpt should be ('defined)
    val model = modelOpt.get
    model.properties.size should be (5)
    val modelStr = write(model)
    val jModel = Serialization.read[JValue](modelStr)
    modelStr should be("""{"id":"ListReply[BoxedTypesIssue31WithDataType]","description":"Standard reply containing a list of items and paging information","properties":{"items":{"type":"array","description":"List of requested items (in current page if paged)","items":{"$ref":"BoxedTypesIssue31WithDataType"}},"total":{"type":"integer","format":"int32","description":"Total number of items in the reply (across all pages if applicable)"},"nextPageToken":{"type":"string","description":"Identifier used to fetch the next page of results"},"offset":{"type":"integer","format":"int32","description":"Offset within the total count of results where this current items list starts"},"limit":{"type":"integer","format":"int32","description":"Limit on the number of items included in a single response page"}}}""")
    val jId = jModel \ "id"
    jId.extract[String] should be (modelName)
    val jRef = jModel \ "properties" \ "items" \ "items" \ "$ref"
    jRef.extract[String] should be (innerName)
  }

  it should "format an ApiDescription with a ListReply[T] responseClass" in {
    val op = Operation(method = "GET",
      summary = "Dummy operation",
      notes = "",
      responseClass = "converter.models.ListReply[converter.models.BoxedTypesIssue31WithDataType]",
      nickname = "GetDummyOperation",
      position = 0)

    val apiDesc = ApiDescription(
      path = "/dummy-operation",
      description = None,
      operations = List(op)
    )

    val apis = List(apiDesc)

    // The lrc is already in from the previous test, but
    // will add again so the test is stand-alone
    val lrc = new ListReplyConverter
    ModelConverters.addConverter(lrc, true)

    val mmOpt = ModelUtil.modelsFromApis(apis)

    mmOpt should be ('defined)

    val mm = mmOpt.get

    val modelNameOpt = ModelUtil.toName(op.responseClass)

    val modelOpt = modelNameOpt.flatMap(n => mm.get(n))

    modelOpt should be ('defined)

    val model = modelOpt.get

    model.name should equal (modelNameOpt.get)

    val innerNameOpt = ModelUtil.splitContainer(modelNameOpt.get) match {
      case (Some(outer), Some(inner)) => Some(inner)
      case _ => None
    }

    innerNameOpt should be ('defined)

    model.properties.size should be (5)
    val modelStr = write(model)
    val jModel = Serialization.read[JValue](modelStr)
    modelStr should be("""{"id":"ListReply[BoxedTypesIssue31WithDataType]","description":"Standard reply containing a list of items and paging information","properties":{"items":{"type":"array","description":"List of requested items (in current page if paged)","items":{"$ref":"BoxedTypesIssue31WithDataType"}},"total":{"type":"integer","format":"int32","description":"Total number of items in the reply (across all pages if applicable)"},"nextPageToken":{"type":"string","description":"Identifier used to fetch the next page of results"},"offset":{"type":"integer","format":"int32","description":"Offset within the total count of results where this current items list starts"},"limit":{"type":"integer","format":"int32","description":"Limit on the number of items included in a single response page"}}}""")
    val jRef = jModel \ "properties" \ "items" \ "items" \ "$ref"
    jRef.extract[String] should equal (innerNameOpt.get)
  }
}

class ListReplyConverter extends SwaggerSchemaConverter with BaseConverter {
  override def read(cls: Class[_], typeMap: Map[String, String] = Map.empty,
                    modelNameOpt: Option[String] = None): Option[Model] = {

    val modelOpt: Option[Model] = super.read(cls, typeMap, modelNameOpt)

    val lrModelOpt = modelOpt.map(m => {
      val (fqOuterOpt, fqInnerOpt)  = modelNameOpt.fold[(Option[String], Option[String])]((None, None))(modelName => ModelUtil.splitContainer(modelName))
      val (outerOpt, innerOpt) = ModelUtil.splitContainer(m.name)
      (outerOpt, innerOpt) match {
        case (Some("ListReply"), Some(inner)) => {
          val modelPropOpt = m.properties.get("items")
          modelPropOpt.fold(m)(p => {
            val modelRefOpt = p.items
            val lrModelRefOpt = modelRefOpt.map(r => {
              if (SwaggerSpec.baseTypes.contains(inner))
                r.copy(`type` = inner, ref = None, qualifiedType = fqInnerOpt)
              else
                r.copy(`type` = null, ref = Some(inner), qualifiedType = fqInnerOpt)
            })
            m.copy(properties = m.properties += (("items", p.copy(items = lrModelRefOpt))))
          })
        }
        case _ => m
      }
    })
    lrModelOpt
  }
}