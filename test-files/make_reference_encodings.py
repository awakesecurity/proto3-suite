import test_pb2

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
    with open('multiple_fields.bin', 'wb') as f:
        f.write(multipleFields.SerializeToString())

    withEnum = test_pb2.WithEnum()
    withEnum.enumField = test_pb2.WithEnum.ENUM1
    with open('with_enum0.bin', 'wb') as f:
        f.write(withEnum.SerializeToString())

    withEnum = test_pb2.WithEnum()
    withEnum.enumField = test_pb2.WithEnum.ENUM2
    with open('with_enum1.bin', 'wb') as f:
        f.write(withEnum.SerializeToString())

    withNesting = test_pb2.WithNesting()
    withNesting.nestedMessage.nestedField1 = "123abc"
    withNesting.nestedMessage.nestedField2 = 123456
    with open('with_nesting.bin', 'wb') as f:
        f.write(withNesting.SerializeToString())

    withRepetition = test_pb2.WithRepetition()
    withRepetition.repeatedField1.extend([1,2,3,4,5])
    with open('with_repetition.bin', 'wb') as f:
        f.write(withRepetition.SerializeToString())

    trivNeg = test_pb2.Trivial()
    trivNeg.trivialField = -1
    with open('trivial_negative.bin', 'wb') as f:
        f.write(trivNeg.SerializeToString())

if __name__ == '__main__':
    main()
