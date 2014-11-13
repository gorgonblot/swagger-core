package converter.models

import com.wordnik.swagger.annotations.{ ApiModel, ApiModelProperty }

import scala.annotation.meta.field

// Issue #31: https://github.com/gettyimages/spray-swagger/issues/31
// It would be nice if the Seq[Int] and Option[Int] could create the proper spec, but due
// to erasure the parameterized types are only identified as Object
@ApiModel(description = "Options of boxed types produces an Object ref instead of correct type")
case class BoxedTypesIssue31(stringSeq: Seq[String], stringOpt: Option[String],
                             @(ApiModelProperty @field)(value = "Integers in a Sequence Box") intSeq: Seq[Int],
                             @(ApiModelProperty @field)(value = "Integer in an Option Box") intOpt: Option[Int],
                             justInt: Int)

// Get around the erasure by providing the dataType explicitly using the dataType common names.
@ApiModel(description = "Options of boxed types produces an Object ref instead of correct type, but can be overcome with dataType")
case class BoxedTypesIssue31WithDataType(stringSeq: Seq[String], stringOpt: Option[String],
                                         @(ApiModelProperty @field)(value = "Integers in a Sequence Box", dataType = "List[int]") intSeq: Seq[Int],
                                         @(ApiModelProperty @field)(value = "Integer in an Option Box", dataType = "int") intOpt: Option[Int],
                                         justInt: Int)

// Get around erasure in a parameterized generic type
@ApiModel(description = "Standard reply containing a list of items and paging information")
case class ListReply[T](
                         @(ApiModelProperty @field)(value = "List of requested items (in current page if paged)") items: List[T],
                         @(ApiModelProperty @field)(value = "Total number of items in the reply (across all pages if applicable)") total: Int,
                         @(ApiModelProperty @field)(value = "Identifier used to fetch the next page of results") nextPageToken: Option[String],
                         @(ApiModelProperty @field)(value = "Offset within the total count of results where this current items list starts") offset: Int,
                         @(ApiModelProperty @field)(value = "Limit on the number of items included in a single response page", dataType = "int") limit: Option[Int]
                         )

object ListReply {
  def apply[T](lst: List[T]): ListReply[T] = new ListReply(lst, lst.length, None, 0, None)
}
