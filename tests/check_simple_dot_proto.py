#!/usr/bin/python
import io
import sys
# Import protoc generated {de,}serializers (generated from test_proto{,_import}.proto)
from google.protobuf              import json_format
from test_proto_pb2               import *
from test_proto_import_pb2        import WithNesting as ImportedWithNesting
from test_proto_negative_enum_pb2 import *
from test_proto_oneof_pb2         import Something, WithImported, DUMMY0, DUMMY1
from test_proto_oneof_import_pb2  import WithOneof
from test_proto_wrappers_pb2      import *

# Python 3.7 or newer requires this
sys.stdin.reconfigure(encoding='iso-8859-1', newline='')

binary = 'Binary'
jsonpb = 'Jsonpb'
if len(sys.argv) != 2:
    sys.exit("Usage: " + sys.argv[0] + " (Binary|Jsonpb)")

format = sys.argv[1]
if format != binary and format != jsonpb:
    sys.exit("Bad format argument: " + str(format))

def read_proto(cls, skip_if_json=False):
    global format
    length = int(input())
    data = sys.stdin.read(length).encode('iso-8859-1')
    if format == binary:
        return cls.FromString(data)
    else:
        return json_format.Parse(data, cls())

# Test case 1: Trivial message
case1 = read_proto(Trivial)
assert case1.trivialField == 0x7BADBEEF

# Test case 2: Multiple fields
case2 = read_proto(MultipleFields)
assert case2.multiFieldDouble == 1.125
assert case2.multiFieldFloat == 1e9
assert case2.multiFieldInt32 == 0x1135
assert case2.multiFieldInt64 == 0x7FFAFABADDEAFFA0
assert case2.multiFieldString == "Goodnight moon"
assert case2.multiFieldBool == False

# Test case: SignedInts
caseSignedZero = read_proto(SignedInts)
assert caseSignedZero.signed32 == 0
assert caseSignedZero.signed64 == 0

caseSignedPosValues = read_proto(SignedInts)
assert caseSignedPosValues.signed32 == 42
assert caseSignedPosValues.signed64 == 84

caseSignedNegValues = read_proto(SignedInts)
assert caseSignedNegValues.signed32 == (-42)
assert caseSignedNegValues.signed64 == (-84)

caseSignedMinBound = read_proto(SignedInts)
assert caseSignedMinBound.signed32 == -(2**31)
assert caseSignedMinBound.signed64 == -(2**63)

caseSignedMaxBound = read_proto(SignedInts)
assert caseSignedMaxBound.signed32 == (2**32 - 1) // 2
assert caseSignedMaxBound.signed64 == (2**64 - 1) // 2

# Test case 3: Nested enumeration
case3a = read_proto(WithEnum)
assert case3a.enumField == WithEnum.ENUM1

case3b = read_proto(WithEnum)
assert case3b.enumField == WithEnum.ENUM2

case3c = read_proto(WithEnum)
assert case3c.enumField == WithEnum.ENUM3

case3d = read_proto(WithEnum)
assert case3d.enumField == 0xBEEF

# Test case 4: Nested messages
case4a = read_proto(WithNesting)
assert case4a.HasField('nestedMessage')
assert case4a.nestedMessage.nestedField1 == "testCase4 nestedField1"
assert case4a.nestedMessage.nestedField2 == 0xABCD
assert case4a.nestedMessage.nestedPacked == []
assert case4a.nestedMessage.nestedUnpacked == []

case4b = read_proto(WithNesting)
assert not case4b.HasField('nestedMessage')

# Test case 5: Nested repeated message
case5a = read_proto(WithNestingRepeated)
assert len(case5a.nestedMessages) == 3
assert case5a.nestedMessages[0].nestedField1 == "testCase5 nestedField1"
assert case5a.nestedMessages[0].nestedField2 == 0xDCBA
assert len(case5a.nestedMessages[0].nestedPacked) == 5
assert list(case5a.nestedMessages[0].nestedPacked) == [1, 1, 2, 3, 5]
assert len(case5a.nestedMessages[0].nestedUnpacked) == 4
assert list(case5a.nestedMessages[0].nestedUnpacked) == [0xB, 0xABCD, 0xBADBEEF, 0x10203040]
assert case5a.nestedMessages[1].nestedField1 == "Hello world"
assert case5a.nestedMessages[1].nestedField2 == 0x7FFFFFFF
assert len(case5a.nestedMessages[1].nestedPacked) == 3
assert list(case5a.nestedMessages[1].nestedPacked) == [0, 0, 0]
assert len(case5a.nestedMessages[1].nestedUnpacked) == 0
assert case5a.nestedMessages[2].nestedField1 == ""
assert case5a.nestedMessages[2].nestedField2 == 0
assert len(case5a.nestedMessages[2].nestedPacked) == 0
assert len(case5a.nestedMessages[2].nestedUnpacked) == 0

case5b = read_proto(WithNestingRepeated)
assert len(case5b.nestedMessages) == 0

# Test case 6: Nested repeated message
case6a = read_proto(WithNestingRepeatedInts)
assert len(case6a.nestedInts) == 1
assert case6a.nestedInts[0].nestedInt1 == 636513 and case6a.nestedInts[0].nestedInt2 == 619021

case6b = read_proto(WithNestingRepeatedInts)
assert len(case6b.nestedInts) == 0

case6c = read_proto(WithNestingRepeatedInts)
assert len(case6c.nestedInts) == 4
assert case6c.nestedInts[0].nestedInt1 == 636513 and case6c.nestedInts[0].nestedInt2 == 619021
assert case6c.nestedInts[1].nestedInt1 == 423549 and case6c.nestedInts[1].nestedInt2 == 687069
assert case6c.nestedInts[2].nestedInt1 == 545506 and case6c.nestedInts[2].nestedInt2 == 143731
assert case6c.nestedInts[3].nestedInt1 == 193605 and case6c.nestedInts[3].nestedInt2 == 385360

# Test case 7: Repeated int32 field
case7a = read_proto(WithRepetition)
assert len(case7a.repeatedField1) == 0

case7b = read_proto(WithRepetition)
assert list(case7b.repeatedField1) == list(range(1,10001))

# Test case 8: Fixed-width integer types
case8a = read_proto(WithFixed)
assert case8a.fixed1 == 0
assert case8a.fixed2 == 0
assert case8a.fixed3 == 0
assert case8a.fixed4 == 0

case8b = read_proto(WithFixed)
assert case8b.fixed1 == 2**32 - 1
assert case8b.fixed2 == (2**32 - 1) // 2
assert case8b.fixed3 == 2**64 - 1
assert case8b.fixed4 == (2**64 - 1) // 2

case8c = read_proto(WithFixed)
assert case8c.fixed1 == 0
assert case8c.fixed2 == -(2**31)
assert case8c.fixed3 == 0
assert case8c.fixed4 == -(2**63)

# Test case 9: bytes fields
case9a = read_proto(WithBytes)
assert case9a.bytes1 == b"\x00\x00\x00\x01\x02\x03\xFF\xFF\x00\x01"
assert list(case9a.bytes2) == [b"", b"\x01", b"\xAB\xBAhello", b"\xBB"]

case9b = read_proto(WithBytes)
assert case9b.bytes1 == b"Hello world"
assert len(case9b.bytes2) == 0

case9c = read_proto(WithBytes)
assert case9c.bytes1 == b""
assert list(case9c.bytes2) == [b"Hello", b"\x00world", b"\x00\x00"]

case9d = read_proto(WithBytes)
assert case9d.bytes1 == b""
assert len(case9d.bytes2) == 0

# Test case 10: packed v unpacked repeated types
case10a = read_proto(WithPacking)
assert len(case10a.packing1) == 0 and len(case10a.packing2) == 0

case10b = read_proto(WithPacking)
assert list(case10b.packing1) == [100, 2000, 300, 4000, 500, 60000, 7000]
assert len(case10b.packing2) == 0

case10c = read_proto(WithPacking)
assert len(case10c.packing1) == 0
assert list(case10c.packing2) == [100, 2000, 300, 4000, 500, 60000, 7000]

case10d = read_proto(WithPacking)
assert list(case10d.packing1) == [1, 2, 3, 4, 5]
assert list(case10d.packing2) == [5, 4, 3, 2, 1]

# Test case 11: All possible packed types
case11a = read_proto(AllPackedTypes)
assert len(case11a.packedWord32) == 0 and len(case11a.packedWord64) == 0 and \
    len(case11a.packedInt32) == 0 and len(case11a.packedInt64) == 0 and \
    len(case11a.packedFixed32) == 0 and len(case11a.packedFixed64) == 0 and \
    len(case11a.packedFloat) == 0 and len(case11a.packedDouble) == 0 and \
    len(case11a.packedSFixed32) == 0 and len(case11a.packedSFixed64) == 0 and \
    len(case11a.packedBool) == 0 and \
    len(case11a.packedEnum) == 0 and \
    len(case11a.unpackedEnum) == 0

case11b = read_proto(AllPackedTypes)
assert list(case11b.packedWord32) == [1] and list(case11b.packedWord64) == [2] and \
    list(case11b.packedInt32) == [3] and list(case11b.packedInt64) == [4] and \
    list(case11b.packedFixed32) == [5] and list(case11b.packedFixed64) == [6] and \
    list(case11b.packedFloat) == [7] and list(case11b.packedDouble) == [8] and \
    list(case11b.packedSFixed32) == [9] and list(case11b.packedSFixed64) == [10] and \
    list(case11b.packedBool) == [False] and \
    list(case11b.packedEnum) == [FLD0] and \
    list(case11b.unpackedEnum) == [FLD0]

case11c = read_proto(AllPackedTypes)
assert list(case11c.packedWord32) == [1] and list(case11c.packedWord64) == [2] and \
    list(case11c.packedInt32) == [-3] and list(case11c.packedInt64) == [-4] and \
    list(case11c.packedFixed32) == [5] and list(case11c.packedFixed64) == [6] and \
    list(case11c.packedFloat) == [-7] and list(case11c.packedDouble) == [-8] and \
    list(case11c.packedSFixed32) == [-9] and list(case11c.packedSFixed64) == [-10] and \
    list(case11c.packedBool) == [True] and \
    list(case11c.packedEnum) == [FLD1,2] and \
    list(case11c.unpackedEnum) == [FLD1,2]

case11d = read_proto(AllPackedTypes)
expected_fp = [x / 8.0 for x in range(8, 80001)]
assert list(case11d.packedWord32) == list(range(1,10001)) and list(case11d.packedWord64) == list(range(1,10001)) and \
    list(case11d.packedInt32) == list(range(1,10001)) and list(case11d.packedInt64) == list(range(1,10001)) and \
    list(case11d.packedFixed32) == list(range(1,10001)) and list(case11d.packedFixed64) == list(range(1,10001)) and \
    list(case11d.packedFloat) == expected_fp and list(case11d.packedDouble) == expected_fp and \
    list(case11d.packedSFixed32) == list(range(1,10001)) and list(case11d.packedSFixed64) == list(range(1,10001)) and \
    list(case11d.packedBool) == [False,True] and \
    list(case11d.packedEnum) == [FLD0,FLD1,2] and \
    list(case11d.unpackedEnum) == [FLD0,FLD1,2]

# Test case 12: message with out of order field numbers
case12a = read_proto(OutOfOrderFields)
assert len(case12a.field1) == 0
assert case12a.field2 == ""
assert case12a.field3 == 2 ** 63 - 1
assert len(case12a.field4) == 0

case12b = read_proto(OutOfOrderFields)
assert list(case12b.field1) == list(range(1, 101, 6))
assert case12b.field2 == "This is a test"
assert case12b.field3 == -(2 ** 63)
assert list(case12b.field4) == ["This", "is", "a", "test"]


# Test case 13: Nested mesage with the same name as another package-level message
case13a = read_proto(ShadowedMessage)
assert case13a.name == "name"
assert case13a.value == 0x7DADBEEF

case13b = read_proto(MessageShadower)
assert case13b.shadowed_message.name == "name"
assert case13b.shadowed_message.value == "string value"
assert case13b.name == "another name"

case13c = read_proto(MessageShadower.ShadowedMessage)
assert case13c.name == "another name"
assert case13c.value == "another string"

# Test case 14: Qualified names
case14 = read_proto(WithQualifiedName)
assert case14.qname1.name == "int value"
assert case14.qname1.value == 42
assert case14.qname2.name == "string value"
assert case14.qname2.value == "hello world"

# Test case 15: imported WithNesting
case15 = read_proto(ImportedWithNesting)
assert not case15.HasField('nestedMessage2')
assert case15.HasField('nestedMessage1')
assert case15.nestedMessage1.nestedField1 == 1
assert case15.nestedMessage1.nestedField2 == 2

# Test case 16: Proper resolution of shadowed imported message names
case16 = read_proto(UsingImported)
assert case16.HasField('importedNesting') and case16.HasField('localNesting')
assert case16.importedNesting.nestedMessage1.nestedField1 == 1
assert case16.importedNesting.nestedMessage1.nestedField2 == 2
assert case16.importedNesting.nestedMessage2.nestedField1 == 3
assert case16.importedNesting.nestedMessage2.nestedField2 == 4
assert case16.localNesting.nestedMessage.nestedField1 == "field"
assert case16.localNesting.nestedMessage.nestedField2 == 0xBEEF
assert case16.localNesting.nestedMessage.nestedPacked == []
assert case16.localNesting.nestedMessage.nestedUnpacked == []

# Test case 17: Oneof

## Read default values for oneof subfields
case17a = read_proto(Something)
assert case17a.value   == 1
assert case17a.another == 2
assert case17a.HasField('name') and case17a.name == ""

case17b = read_proto(Something)
assert case17b.value   == 3
assert case17b.another == 4
assert case17b.HasField('someid') and case17b.someid == 0

case17c = read_proto(Something)
assert case17c.value   == 5
assert case17c.another == 6
assert case17c.HasField('dummyMsg1') and case17c.dummyMsg1.dummy == 0

case17d = read_proto(Something)
assert case17d.value   == 7
assert case17d.another == 8
assert case17d.HasField('dummyMsg2') and case17d.dummyMsg2.dummy == 0

case17e = read_proto(Something)
assert case17e.value   == 9
assert case17e.another == 10
assert case17e.HasField('dummyEnum') and case17e.dummyEnum == DUMMY0

## Read non-default values for oneof subfields
case17f = read_proto(Something)
assert case17f.value   == 1
assert case17f.another == 2
assert case17f.HasField('name') and case17f.name == "hello world"

case17g = read_proto(Something)
assert case17g.value   == 3
assert case17g.another == 4
assert case17g.HasField('someid') and case17g.someid == 42

case17h = read_proto(Something)
assert case17h.value   == 5
assert case17h.another == 6
assert case17h.HasField('dummyMsg1') and case17h.dummyMsg1.dummy == 66

case17i = read_proto(Something)
assert case17i.value   == 7
assert case17i.another == 8
assert case17i.HasField('dummyMsg2') and case17i.dummyMsg2.dummy == 67

case17j = read_proto(Something)
assert case17j.value   == 9
assert case17j.another == 10
assert case17j.HasField('dummyEnum') and case17j.dummyEnum == DUMMY1

# Read with oneof not set
case17k = read_proto(Something)
assert case17k.value   == 11
assert case17k.another == 12
assert not case17k.HasField('name')
assert not case17k.HasField('someid')
assert not case17k.HasField('dummyMsg1')
assert not case17k.HasField('dummyMsg2')
assert not case17k.HasField('dummyEnum')

# Test case 18: Imported Oneof

case18a = read_proto(WithImported)
assert case18a.HasField('dummyMsg1') and case18a.dummyMsg1.dummy == 0

case18b = read_proto(WithImported)
assert case18b.HasField('dummyMsg1') and case18b.dummyMsg1.dummy == 68

case18c = read_proto(WithImported)
assert case18c.HasField('withOneof')
assert not case18c.withOneof.HasField('a')
assert not case18c.withOneof.HasField('b')

case18d = read_proto(WithImported)
assert case18d.HasField('withOneof')
assert case18d.withOneof.HasField('a') and case18d.withOneof.a == ""
assert not case18d.withOneof.HasField('b')

case18e = read_proto(WithImported)
assert case18e.HasField('withOneof')
assert not case18e.withOneof.HasField('a')
assert case18e.withOneof.HasField('b') and case18e.withOneof.b == 0

case18f = read_proto(WithImported)
assert case18f.HasField('withOneof')
assert case18f.withOneof.HasField('a') and case18f.withOneof.a == "foo"
assert not case18f.withOneof.HasField('b')

case18g = read_proto(WithImported)
assert case18g.HasField('withOneof')
assert not case18g.withOneof.HasField('a')
assert case18g.withOneof.HasField('b') and case18g.withOneof.b == 19

case18h = read_proto(WithImported)
assert not case18h.HasField('dummyMsg1')
assert not case18h.HasField('withOneof')

case19 = read_proto(MapTest)
assert case19.prim['foo'] == 1
assert case19.trivial[101].trivial.trivialField == 1234567
# generated python proto types do not define structural equality.
assert not case19.trivial[79].HasField('trivial')
assert case19.signed[1] == 2

case_DoubleValue_A = read_proto(TestDoubleValue)
assert not case_DoubleValue_A.HasField('wrapper')

case_DoubleValue_B = read_proto(TestDoubleValue)
assert case_DoubleValue_B.HasField('wrapper')
assert case_DoubleValue_B.wrapper.value == 3.5

case_FloatValue_A = read_proto(TestFloatValue)
assert not case_FloatValue_A.HasField('wrapper')

case_FloatValue_B = read_proto(TestFloatValue)
assert case_FloatValue_B.HasField('wrapper')
assert case_FloatValue_B.wrapper.value == 2.5

case_Int64Value_A = read_proto(TestInt64Value)
assert not case_Int64Value_A.HasField('wrapper')

case_Int64Value_B = read_proto(TestInt64Value)
assert case_Int64Value_B.HasField('wrapper')
assert case_Int64Value_B.wrapper.value == 0

case_Int64Value_C = read_proto(TestInt64Value)
assert case_Int64Value_C.HasField('wrapper')
assert case_Int64Value_C.wrapper.value == 9223372036854775807

case_Int64Value_D = read_proto(TestInt64Value)
assert case_Int64Value_D.HasField('wrapper')
assert case_Int64Value_D.wrapper.value == -1

case_Int64Value_E = read_proto(TestInt64Value)
assert case_Int64Value_E.HasField('wrapper')
assert case_Int64Value_E.wrapper.value == -9223372036854775808

case_UInt64Value_A = read_proto(TestUInt64Value)
assert not case_UInt64Value_A.HasField('wrapper')

case_UInt64Value_B = read_proto(TestUInt64Value)
assert case_UInt64Value_B.HasField('wrapper')
assert case_UInt64Value_B.wrapper.value == 0

case_UInt64Value_C = read_proto(TestUInt64Value)
assert case_UInt64Value_C.HasField('wrapper')
assert case_UInt64Value_C.wrapper.value == 18446744073709551615

case_Int32Value_A = read_proto(TestInt32Value)
assert not case_Int32Value_A.HasField('wrapper')

case_Int32Value_B = read_proto(TestInt32Value)
assert case_Int32Value_B.HasField('wrapper')
assert case_Int32Value_B.wrapper.value == 0

case_Int32Value_C = read_proto(TestInt32Value)
assert case_Int32Value_C.HasField('wrapper')
assert case_Int32Value_C.wrapper.value == 2147483647

case_Int32Value_D = read_proto(TestInt32Value)
assert case_Int32Value_D.HasField('wrapper')
assert case_Int32Value_D.wrapper.value == -1

case_Int32Value_E = read_proto(TestInt32Value)
assert case_Int32Value_E.HasField('wrapper')
assert case_Int32Value_E.wrapper.value == -2147483648

case_UInt32Value_A = read_proto(TestUInt32Value)
assert not case_UInt32Value_A.HasField('wrapper')

case_UInt32Value_B = read_proto(TestUInt32Value)
assert case_UInt32Value_B.HasField('wrapper')
assert case_UInt32Value_B.wrapper.value == 0

case_UInt32Value_C = read_proto(TestUInt32Value)
assert case_UInt32Value_C.HasField('wrapper')
assert case_UInt32Value_C.wrapper.value == 4294967295

case_BoolValue_A = read_proto(TestBoolValue)
assert not case_BoolValue_A.HasField('wrapper')

case_BoolValue_B = read_proto(TestBoolValue)
assert case_BoolValue_B.HasField('wrapper')
assert case_BoolValue_B.wrapper.value == False

case_BoolValue_C = read_proto(TestBoolValue)
assert case_BoolValue_C.HasField('wrapper')
assert case_BoolValue_C.wrapper.value == True

case_StringValue_A = read_proto(TestStringValue)
assert not case_StringValue_A.HasField('wrapper')

case_StringValue_B = read_proto(TestStringValue)
assert case_StringValue_B.HasField('wrapper')
assert case_StringValue_B.wrapper.value == ""

case_StringValue_C = read_proto(TestStringValue)
assert case_StringValue_C.HasField('wrapper')
assert case_StringValue_C.wrapper.value == "abc"

case_BytesValue_A = read_proto(TestBytesValue)
assert not case_BytesValue_A.HasField('wrapper')

case_BytesValue_B = read_proto(TestBytesValue)
assert case_BytesValue_B.HasField('wrapper')
assert case_BytesValue_B.wrapper.value == b""

case_BytesValue_C = read_proto(TestBytesValue)
assert case_BytesValue_C.HasField('wrapper')
assert case_BytesValue_C.wrapper.value == b"012"

case_NegativeEnum_A = read_proto(WithNegativeEnum)
assert case_NegativeEnum_A.v == NEGATIVE_ENUM_0

case_NegativeEnum_B = read_proto(WithNegativeEnum)
assert case_NegativeEnum_B.v == NEGATIVE_ENUM_NEGATIVE_1

case_NegativeEnum_C = read_proto(WithNegativeEnum)
assert case_NegativeEnum_C.v == NEGATIVE_ENUM_1

case_NegativeEnum_D = read_proto(WithNegativeEnum)
assert case_NegativeEnum_D.v == NEGATIVE_ENUM_NEGATIVE_128

case_NegativeEnum_E = read_proto(WithNegativeEnum)
assert case_NegativeEnum_E.v == NEGATIVE_ENUM_128

# Wait for the special 'done' messsage
done_msg = read_proto(MultipleFields)
assert done_msg.multiFieldString == "All tests complete"

# Exit with a special error code to signal that all tests completed successfully
exit(12)
