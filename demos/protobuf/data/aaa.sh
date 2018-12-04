rm test1.protobuf
rm test2.protobuf
rm test3.protobuf

echo -ne "$(printf '\\x%x' 8)" >> test1.protobuf
echo -ne "$(printf '\\x%x' 150)" >> test1.protobuf
echo -ne "$(printf '\\x%x' 1)" >> test1.protobuf

#12 07 74 65 73 74 69 6e 67
#12 07 74 #65 73 74 #69 6e 67

echo -ne "$(printf '\\x%x' 18)"  >> test2.protobuf
echo -ne "$(printf '\\x%x' 7)"   >> test2.protobuf
echo -ne "$(printf '\\x%x' 116)" >> test2.protobuf

echo -ne "$(printf '\\x%x' 101)" >> test2.protobuf
echo -ne "$(printf '\\x%x' 115)" >> test2.protobuf
echo -ne "$(printf '\\x%x' 116)" >> test2.protobuf

echo -ne "$(printf '\\x%x' 105)" >> test2.protobuf
echo -ne "$(printf '\\x%x' 110)" >> test2.protobuf
echo -ne "$(printf '\\x%x' 103)" >> test2.protobuf



#1a 03 08 96 01
echo -ne "$(printf '\\x%x' 26)"  >> test3.protobuf
echo -ne "$(printf '\\x%x' 3)" >> test3.protobuf

echo -ne "$(printf '\\x%x' 8)" >> test3.protobuf
echo -ne "$(printf '\\x%x' 150)" >> test3.protobuf
echo -ne "$(printf '\\x%x' 1)" >> test3.protobuf
