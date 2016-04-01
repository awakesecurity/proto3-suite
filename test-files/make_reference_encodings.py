# Generates the test-files/*.bin files used in the unit tests.
import test_pb2

def serialize_to_file(msg, fp):
    with open(fp, 'wb') as f:
        f.write(msg.SerializeToString())

def main():
    triv = test_pb2.Trivial()
    triv.trivialField = 123
    with open('trivial.bin', 'wb') as f:
        f.write(triv.SerializeToString())

    multipleFields = test_pb2.MultipleFields()
    multipleFields.multiFieldDouble = 1.23
    multipleFields.multiFieldFloat = -0.5
    multipleFields.multiFieldInt32 = 123
    multipleFields.multiFieldInt64 = 1234567890
    multipleFields.multiFieldString = "Hello, world!"
    serialize_to_file(multipleFields, 'multiple_fields.bin')

    withEnum = test_pb2.WithEnum()
    withEnum.enumField = test_pb2.WithEnum.ENUM1
    serialize_to_file(withEnum, 'with_enum0.bin')

    withEnum = test_pb2.WithEnum()
    withEnum.enumField = test_pb2.WithEnum.ENUM2
    serialize_to_file(withEnum, 'with_enum1.bin')

    withNesting = test_pb2.WithNesting()
    withNesting.nestedMessage.nestedField1 = "123abc"
    withNesting.nestedMessage.nestedField2 = 123456
    serialize_to_file(withNesting, 'with_nesting.bin')

    withRepetition = test_pb2.WithRepetition()
    withRepetition.repeatedField1.extend([1,2,3,4,5])
    serialize_to_file(withRepetition, 'with_repetition.bin')

    trivNeg = test_pb2.Trivial()
    trivNeg.trivialField = -1
    serialize_to_file(trivNeg, 'trivial_negative.bin')

if __name__ == '__main__':
    main()
