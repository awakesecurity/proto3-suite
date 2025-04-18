#!/usr/bin/python
import sys
import os
# Import protoc generated {de,}serializers (generated from test_proto{,_import}.proto)
from google.protobuf                import json_format
from google.protobuf.wrappers_pb2   import *
from test_proto_pb2                 import *
import test_proto_import_pb2        as test_proto_import
import test_proto_negative_enum_pb2 as test_proto_negative_enum
import test_proto_oneof_pb2         as test_proto_oneof
import test_proto_oneof_import_pb2  as test_proto_oneof_import
import test_proto_wrappers_pb2      as test_proto_wrappers

# Python 3.7 or newer requires this
sys.stdout.reconfigure(encoding='iso-8859-1')

binary = 'Binary'
jsonpb = 'Jsonpb'
if len(sys.argv) != 2:
    sys.exit("Usage: " + sys.argv[0] + " (Binary|Jsonpb)")

format = sys.argv[1]
if format != binary and format != jsonpb:
    sys.exit("Bad format argument: " + str(format))

def write_proto(msg):
    if format == binary:
        out = str(msg.SerializeToString(), encoding='iso-8859-1')
    else:
        out = json_format.MessageToJson(msg)
    print(len(out))
    sys.stdout.write(out)

# Test case 1: Trivial message
write_proto(Trivial(trivialField = 0x7BADBEEF))

# Test case 2: Multiple fields
write_proto(
    MultipleFields(
        multiFieldDouble = 1.125,
        multiFieldFloat = 1e9,
        multiFieldInt32 = 0x1135,
        multiFieldInt64 = 0x7FFAFABADDEAFFA0,
        multiFieldString = "Goodnight moon",
        multiFieldBool = False))

# Test case: SignedInts
write_proto(SignedInts(signed32 = 0, signed64 = 0))
write_proto(SignedInts(signed32 = 42, signed64 = 84))
write_proto(SignedInts(signed32 = (-42), signed64 = (-84)))
write_proto(SignedInts(signed32 = -(2**31), signed64 = -(2**63)))
write_proto(SignedInts(signed32 = (2**32 - 1) // 2, signed64 = (2**64 - 1) // 2))

# Test case: RepeatedSignedInts
write_proto(WithRepeatedSigned(r32 = [], r64 = []))
write_proto(WithRepeatedSigned(r32 = [0], r64 = [0]))
write_proto(WithRepeatedSigned(r32 = [0, 42, -42, 2**30 - 1, -(2**30), 2**31 - 1, -(2**31)],
                               r64 = [0, 84, -84, 2**62 - 1, -(2**62), 2**63 - 1, -(2**63)]))

# Test case 3: Nested enumeration
write_proto(WithEnum(enumField = WithEnum.ENUM1))
write_proto(WithEnum(enumField = WithEnum.ENUM2))
write_proto(WithEnum(enumField = WithEnum.ENUM3))
write_proto(WithEnum(enumField = 0xBEEF))

# Test case 4: Nested messages
write_proto(
    WithNesting(nestedMessage=
                WithNesting.Nested(nestedField1   = "testCase4 nestedField1",
                                   nestedField2   = 0xABCD,
                                   nestedPacked   = [],
                                   nestedUnpacked = [])))
write_proto(WithNesting())
write_proto(
    WithNesting(nestedMessage=
                WithNesting.Nested(nestedField1   = "",
                                   nestedField2   = 0,
                                   nestedPacked   = [],
                                   nestedUnpacked = [])))

# Test case 5: Nested repeated message
write_proto(WithNestingRepeated(nestedMessages =
    [ WithNestingRepeated.Nested(nestedField1 = "testCase5 nestedField1",
                                 nestedField2 = 0xDCBA,
                                 nestedPacked = [1, 1, 2, 3, 5],
                                 nestedUnpacked = [0xB, 0xABCD, 0xBADBEEF, 0x10203040]),
      WithNestingRepeated.Nested(nestedField1 = "Hello world",
                                 nestedField2 = 0x7FFFFFFF,
                                 nestedPacked = [0, 0, 0],
                                 nestedUnpacked = []),
      WithNestingRepeated.Nested(nestedField1 = "", nestedField2 = 0,
                                 nestedPacked = [], nestedUnpacked = []) ]))

write_proto(WithNestingRepeated())

# Test case 6: Nested repeated int message
write_proto(WithNestingRepeatedInts(nestedInts=[NestedInts(nestedInt1 = 636513, nestedInt2 = 619021)]))
write_proto(WithNestingRepeatedInts(nestedInts=[]))
write_proto(WithNestingRepeatedInts(nestedInts=
    [ NestedInts(nestedInt1 = 636513, nestedInt2 = 619021),
      NestedInts(nestedInt1 = 423549, nestedInt2 = 687069),
      NestedInts(nestedInt1 = 545506, nestedInt2 = 143731),
      NestedInts(nestedInt1 = 193605, nestedInt2 = 385360) ]))

# Test case 7: Repeated int32 field
write_proto(WithRepetition())
write_proto(WithRepetition(repeatedField1 = range(1, 10001)))

# Test case 8: Fixed-width integer types
write_proto(WithFixed(fixed1 = 0, fixed2 = 0, fixed3 = 0, fixed4 = 0))
write_proto(WithFixed(fixed1 = 2**32 - 1,
                      fixed2 = (2**32 - 1) // 2,
                      fixed3 = 2**64 - 1,
                      fixed4 = (2**64 - 1) // 2))
write_proto(WithFixed(fixed1 = 0,
                      fixed2 = -(2**31),
                      fixed3 = 0,
                      fixed4 = -(2**63)))

# Test case 9: bytes fields
write_proto(WithBytes(bytes1 = b"\x00\x00\x00\x01\x02\x03\xFF\xFF\x00\x01",
                      bytes2 = [b"", b"\x01", b"\xAB\xBAhello", b"\xBB"]))
write_proto(WithBytes(bytes1 = b"Hello world", bytes2 = []))
write_proto(WithBytes(bytes1 = b"", bytes2 = [b"Hello", b"\x00world", b"\x00\x00"]))
write_proto(WithBytes(bytes1 = b"", bytes2 = []))

# Test case 10: packed v unpacked repeated types
write_proto(WithPacking(packing1 = [], packing2 = []))
write_proto(WithPacking(packing1 = [100, 2000, 300, 4000, 500, 60000, 7000],
                        packing2 = []))
write_proto(WithPacking(packing1 = [],
                        packing2 = [100, 2000, 300, 4000, 500, 60000, 7000]))
write_proto(WithPacking(packing1 = [1, 2, 3, 4, 5],
                        packing2 = [5, 4, 3, 2, 1]))

# Test case 11: All possible packed types
write_proto(AllPackedTypes(packedWord32 = [],
                           packedWord64 = [],
                           packedInt32 = [],
                           packedInt64 = [],
                           packedFixed32 = [],
                           packedFixed64 = [],
                           packedFloat = [],
                           packedDouble = [],
                           packedSFixed32 = [],
                           packedSFixed64 = [],
                           packedBool = [],
                           packedEnum = [],
                           unpackedEnum = []))
write_proto(AllPackedTypes(packedWord32 = [1],
                           packedWord64 = [2],
                           packedInt32 = [3],
                           packedInt64 = [4],
                           packedFixed32 = [5],
                           packedFixed64 = [6],
                           packedFloat = [7],
                           packedDouble = [8],
                           packedSFixed32 = [9],
                           packedSFixed64 = [10],
                           packedBool = [False],
                           packedEnum = [FLD0],
                           unpackedEnum = [FLD0]))
write_proto(AllPackedTypes(packedWord32 = [1],
                           packedWord64 = [2],
                           packedInt32 = [-3],
                           packedInt64 = [-4],
                           packedFixed32 = [5],
                           packedFixed64 = [6],
                           packedFloat = [-7],
                           packedDouble = [-8],
                           packedSFixed32 = [-9],
                           packedSFixed64 = [-10],
                           packedBool = [True],
                           packedEnum = [FLD1,2],
                           unpackedEnum = [FLD1,2]))
expected_fp = [x / 8.0 for x in range(8, 80001)]
write_proto(AllPackedTypes(packedWord32 = range(1, 10001),
                           packedWord64 = range(1, 10001),
                           packedInt32 = range(1, 10001),
                           packedInt64 = range(1, 10001),
                           packedFixed32 = range(1, 10001),
                           packedFixed64 = range(1, 10001),
                           packedFloat = expected_fp,
                           packedDouble = expected_fp,
                           packedSFixed32 = range(1, 10001),
                           packedSFixed64 = range(1, 10001),
                           packedBool = [False,True],
                           packedEnum = [FLD0,FLD1,2],
                           unpackedEnum = [FLD0,FLD1,2]))

# Test case 12: message with out of order field numbers
write_proto(OutOfOrderFields(field1 = [], field2 = "", field3 = 2 ** 63 - 1, field4 = []))
write_proto(OutOfOrderFields(field1 = range(1, 101, 6), field2 = "This is a test", field3 = -(2 ** 63), field4 = ["This", "is", "a", "test"]))

# Test case 13: Nested mesage with the same name as another package-level message
write_proto(ShadowedMessage(name = "name", value = 0x7DADBEEF))
write_proto(MessageShadower(shadowed_message = MessageShadower.ShadowedMessage(name = "name", value = "string value"),
                            name = "another name"))
write_proto(MessageShadower.ShadowedMessage(name = "another name", value = "another string"))

# Test case 14: Qualified name resolution
write_proto(WithQualifiedName(qname1 = ShadowedMessage(name="int value", value=42),
                              qname2 = MessageShadower.ShadowedMessage(name="string value", value="hello world")))

# Test case 15: Imported message resolution
write_proto(test_proto_import.WithNesting(nestedMessage1 = test_proto_import.WithNesting.Nested(nestedField1 = 1, nestedField2 = 2)))

# Test case 16: Proper resolution of shadowed message names
write_proto(UsingImported(importedNesting = test_proto_import.WithNesting(nestedMessage1 = test_proto_import.WithNesting.Nested(nestedField1 = 1, nestedField2 = 2),
                                                                          nestedMessage2 = test_proto_import.WithNesting.Nested(nestedField1 = 3, nestedField2 = 4)),
                          localNesting = WithNesting(nestedMessage = WithNesting.Nested(nestedField1 = "field", nestedField2 = 0xBEEF, nestedPacked = [], nestedUnpacked = []))))

# Test case 17: Oneof

# Send default values for oneof subfields
write_proto(test_proto_oneof.Something(value=1, another=2, name=""))
write_proto(test_proto_oneof.Something(value=3, another=4, someid=0))
write_proto(test_proto_oneof.Something(value=5, another=6, dummyMsg1=test_proto_oneof.DummyMsg(dummy=0)))
write_proto(test_proto_oneof.Something(value=7, another=8, dummyMsg2=test_proto_oneof.DummyMsg(dummy=0)))
write_proto(test_proto_oneof.Something(value=9, another=10, dummyEnum=test_proto_oneof.DUMMY0))

# Send non-default values for oneof subfields
write_proto(test_proto_oneof.Something(value=1, another=2, name="hello world"))
write_proto(test_proto_oneof.Something(value=3, another=4, someid=42))
write_proto(test_proto_oneof.Something(value=5, another=6, dummyMsg1=test_proto_oneof.DummyMsg(dummy=66)))
write_proto(test_proto_oneof.Something(value=7, another=8, dummyMsg2=test_proto_oneof.DummyMsg(dummy=67)))
write_proto(test_proto_oneof.Something(value=9, another=10, dummyEnum=test_proto_oneof.DUMMY1))

# Send with oneof not set
write_proto(test_proto_oneof.Something(value=11, another=12))

# Test case 18: Imported Oneof
write_proto(test_proto_oneof.WithImported(dummyMsg1=test_proto_oneof.DummyMsg(dummy=0)))
write_proto(test_proto_oneof.WithImported(dummyMsg1=test_proto_oneof.DummyMsg(dummy=68)))
write_proto(test_proto_oneof.WithImported(withOneof=test_proto_oneof_import.WithOneof()))
write_proto(test_proto_oneof.WithImported(withOneof=test_proto_oneof_import.WithOneof(a="")))
write_proto(test_proto_oneof.WithImported(withOneof=test_proto_oneof_import.WithOneof(b=0)))
write_proto(test_proto_oneof.WithImported(withOneof=test_proto_oneof_import.WithOneof(a="foo")))
write_proto(test_proto_oneof.WithImported(withOneof=test_proto_oneof_import.WithOneof(b=19)))
write_proto(test_proto_oneof.WithImported())


# Test case 19: Maps
if format == jsonpb:
    case19Map = MapTest(
        prim={'foo': 1, 'bar': 42, 'baz': 1234567 },
        trivial={ 1: WrappedTrivial(trivial=Trivial(trivialField=1)),
                  2: WrappedTrivial(trivial=Trivial(trivialField=42)),
                  101: WrappedTrivial(trivial=Trivial(trivialField=1234567)),
                  79: WrappedTrivial(),
                 },
        signed={ 1: 2, 3: 4, 5: 6 }
    )
else:
    # In Binary format we can work around the limitations of Python protobuf
    # support by using an alternative message definition whose binary encoding
    # should be identical, according to the protobuf specification.
    case19Map = MapTestEmulation(
        prim=[
            MapTestEmulation.Prim(key='bar', value=42),
            MapTestEmulation.Prim(key='baz', value=1234567),
            MapTestEmulation.Prim(key='foo', value=1)
        ],
        trivial=[
            MapTestEmulation.Trivial(key=1, value=WrappedTrivial(trivial=Trivial(trivialField=1))),
            MapTestEmulation.Trivial(key=2, value=WrappedTrivial(trivial=Trivial(trivialField=42))),
            MapTestEmulation.Trivial(key=79, value=WrappedTrivial()),
            MapTestEmulation.Trivial(key=80),
            MapTestEmulation.Trivial(key=101, value=WrappedTrivial(trivial=Trivial(trivialField=1234567))),
        ],
        signed=[
            MapTestEmulation.Signed(key=1, value=2),
            MapTestEmulation.Signed(key=3, value=4),
            MapTestEmulation.Signed(key=5, value=6)
        ]
    )
write_proto(case19Map)

# Test DoubleValue
write_proto(test_proto_wrappers.TestDoubleValue
            (many=map(lambda x: DoubleValue(value=x), [4.75, 0.0, -7.125])))
write_proto(test_proto_wrappers.TestDoubleValue
            (wrapper=DoubleValue(value=3.5),
             one=DoubleValue(value=0.0)))
write_proto(test_proto_wrappers.TestDoubleValue
            (wrapper=DoubleValue(value=-3.5),
             many=map(lambda x: DoubleValue(value=x), [0.0, 0.0, 0.0]),
             one=DoubleValue(value=-1.75)))

# Test FloatValue
write_proto(test_proto_wrappers.TestFloatValue(one=FloatValue(value=0.0)))
write_proto(test_proto_wrappers.TestFloatValue
            (wrapper=FloatValue(value=2.5),
             many=map(lambda x: FloatValue(value=x), [1.75, 0.0, -5.125])))
write_proto(test_proto_wrappers.TestFloatValue
            (wrapper=FloatValue(value=-2.5),
             many=map(lambda x: FloatValue(value=x), [0.0, 0.0, 0.0]),
             one=FloatValue(value=-1.25)))

# Test Int64Value
write_proto(test_proto_wrappers.TestInt64Value
            (many=map(lambda x: Int64Value(value=x), [1, 0, -5])))
write_proto(test_proto_wrappers.TestInt64Value
            (wrapper=Int64Value(value=0),
             one=Int64Value(value=5)))
write_proto(test_proto_wrappers.TestInt64Value
            (wrapper=Int64Value(value=9223372036854775807),
             many=map(lambda x: Int64Value(value=x), [-9223372036854775808, 0, 9223372036854775807]),
             one=Int64Value(value=-9223372036854775808)))
write_proto(test_proto_wrappers.TestInt64Value
            (wrapper=Int64Value(value=-1),
             many=map(lambda x: Int64Value(value=x), [0, 9223372036854775807, -9223372036854775808]),
             one=Int64Value(value=9223372036854775807)))
write_proto(test_proto_wrappers.TestInt64Value
            (wrapper=Int64Value(value=-9223372036854775808),
             many=map(lambda x: Int64Value(value=x), [0, 0, 0]),
             one=Int64Value(value=0)))

# Test UInt64Value
write_proto(test_proto_wrappers.TestUInt64Value
            (many=map(lambda x: UInt64Value(value=x), [1, 0, 5])))
write_proto(test_proto_wrappers.TestUInt64Value
            (wrapper=UInt64Value(value=0),
             one=UInt64Value(value=5)))
write_proto(test_proto_wrappers.TestUInt64Value
            (wrapper=UInt64Value(value=18446744073709551615),
             many=map(lambda x: UInt64Value(value=x), [0, 0, 18446744073709551615]),
             one=UInt64Value(value=0)))
write_proto(test_proto_wrappers.TestUInt64Value
            (wrapper=UInt64Value(value=1),
             many=map(lambda x: UInt64Value(value=x), [0, 18446744073709551615, 0]),
             one=UInt64Value(value=18446744073709551615)))
write_proto(test_proto_wrappers.TestUInt64Value
            (wrapper=UInt64Value(value=0),
             many=map(lambda x: UInt64Value(value=x), [0, 0, 0]),
             one=UInt64Value(value=0)))

# Test Int32Value
write_proto(test_proto_wrappers.TestInt32Value
            (many=map(lambda x: Int32Value(value=x), [1, 0, -5])))
write_proto(test_proto_wrappers.TestInt32Value
            (wrapper=Int32Value(value=0),
             one=Int32Value(value=5)))
write_proto(test_proto_wrappers.TestInt32Value
            (wrapper=Int32Value(value=2147483647),
             many=map(lambda x: Int32Value(value=x), [-2147483648, 0, 2147483647]),
             one=Int32Value(value=-2147483648)))
write_proto(test_proto_wrappers.TestInt32Value
            (wrapper=Int32Value(value=-1),
             many=map(lambda x: Int32Value(value=x), [0, 2147483647, -2147483648]),
             one=Int32Value(value=2147483647)))
write_proto(test_proto_wrappers.TestInt32Value
            (wrapper=Int32Value(value=-2147483648),
             many=map(lambda x: Int32Value(value=x), [0, 0, 0]),
             one=Int32Value(value=0)))

# Test UInt32Value
write_proto(test_proto_wrappers.TestUInt32Value
            (many=map(lambda x: UInt32Value(value=x), [1, 0, 5])))
write_proto(test_proto_wrappers.TestUInt32Value
            (wrapper=UInt32Value(value=0),
             one=UInt32Value(value=5)))
write_proto(test_proto_wrappers.TestUInt32Value
            (wrapper=UInt32Value(value=4294967295),
             many=map(lambda x: UInt32Value(value=x), [0, 0, 4294967295]),
             one=UInt32Value(value=0)))
write_proto(test_proto_wrappers.TestUInt32Value
            (wrapper=UInt32Value(value=1),
             many=map(lambda x: UInt32Value(value=x), [0, 4294967295, 0]),
             one=UInt32Value(value=4294967295)))
write_proto(test_proto_wrappers.TestUInt32Value
            (wrapper=UInt32Value(value=0),
             many=map(lambda x: UInt32Value(value=x), [0, 0, 0]),
             one=UInt32Value(value=0)))

# Test BoolValue
write_proto(test_proto_wrappers.TestBoolValue
            (many=map(lambda x: BoolValue(value=x), [False, True])))
write_proto(test_proto_wrappers.TestBoolValue
            (wrapper=BoolValue(value=False),
             one=BoolValue(value=True)))
write_proto(test_proto_wrappers.TestBoolValue
            (wrapper=BoolValue(value=True),
             many=map(lambda x: BoolValue(value=x), [True, False]),
             one=BoolValue(value=False)))

# Test StringValue
write_proto(test_proto_wrappers.TestStringValue
            (many=map(lambda x: StringValue(value=x), ["abc", "", "def"])))
write_proto(test_proto_wrappers.TestStringValue
            (wrapper=StringValue(value=""),
             one=StringValue(value="xyz")))
write_proto(test_proto_wrappers.TestStringValue
            (wrapper=StringValue(value="abc"),
             many=map(lambda x: StringValue(value=x), ["", "", ""]),
             one=StringValue(value="")))

# Test BytesValue
write_proto(test_proto_wrappers.TestBytesValue
            (many=map(lambda x: BytesValue(value=x), [b"012", b"", b"345"])))
write_proto(test_proto_wrappers.TestBytesValue
            (wrapper=BytesValue(value=b""),
             one=BytesValue(value=b"789")))
write_proto(test_proto_wrappers.TestBytesValue
            (wrapper=BytesValue(value=b"012"),
             many=map(lambda x: BytesValue(value=x), [b"", b"", b""]),
             one=BytesValue(value=b"")))

# Test NegativeEnum
write_proto(test_proto_negative_enum.WithNegativeEnum(v=test_proto_negative_enum.NEGATIVE_ENUM_0))
write_proto(test_proto_negative_enum.WithNegativeEnum(v=test_proto_negative_enum.NEGATIVE_ENUM_NEGATIVE_1))
write_proto(test_proto_negative_enum.WithNegativeEnum(v=test_proto_negative_enum.NEGATIVE_ENUM_1))
write_proto(test_proto_negative_enum.WithNegativeEnum(v=test_proto_negative_enum.NEGATIVE_ENUM_NEGATIVE_128))
write_proto(test_proto_negative_enum.WithNegativeEnum(v=test_proto_negative_enum.NEGATIVE_ENUM_128))

# Send the special 'done' message
write_proto(MultipleFields(multiFieldString = "All tests complete"))
