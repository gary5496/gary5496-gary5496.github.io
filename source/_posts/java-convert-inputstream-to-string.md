---
title: Convert InputStream to String (Java)
tags: [Java]
date: 2018-03-16 16:44:03
categories: Java
---
### Background

Sometimes, you would need to convert InputStream to String, and then manipulate it, such as check the String in the trace log, etc.

Here are some ways to convert an InputStream to a String. See below link for details.

<https://stackoverflow.com/questions/309424/read-convert-an-inputstream-to-a-string>

### Ways to convert an InputStream to a String

- Using IOUtils.toString (`Apache Utils`)

``` java
String result = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
```

``` java
String result = IOUtils.toString(inputStream, "UTF-8");
```

- Using CharStreams (`guava`)

``` java
String result = CharStreams.toString(new InputStreamReader(
      inputStream, Charsets.UTF_8));
```

- Using Scanner (`JDK`)

``` java
Scanner s = new Scanner(inputStream).useDelimiter("\\A");
String result = s.hasNext() ? s.next() : "";
```

- Using Stream Api (`Java 8`). Warning: This solution convert different line breaks (like `\r\n`) to `\n`.

``` java
String result = new BufferedReader(new InputStreamReader(inputStream))
  .lines().collect(Collectors.joining("\n"));
```

- Using parallel Stream Api (`Java 8`). Warning: This solution convert different line breaks (like `\r\n`) to `\n`.

``` java
String result = new BufferedReader(new InputStreamReader(inputStream)).lines()
   .parallel().collect(Collectors.joining("\n"));
```

- Using InputStreamReader and StringBuilder (`JDK`)

``` java
final int bufferSize = 1024;
final char[] buffer = new char[bufferSize];
final StringBuilder out = new StringBuilder();
Reader in = new InputStreamReader(inputStream, "UTF-8");
for (; ; ) {
    int rsz = in.read(buffer, 0, buffer.length);
    if (rsz < 0)
        break;
    out.append(buffer, 0, rsz);
}
return out.toString();
```

- Using StringWriter and IOUtils.copy (`Apache Commons`)

``` java
StringWriter writer = new StringWriter();
IOUtils.copy(inputStream, writer, "UTF-8");
return writer.toString();
```

- Using ByteArrayOutputStream and inputStream.read (`JDK`)

``` java
ByteArrayOutputStream result = new ByteArrayOutputStream();
byte[] buffer = new byte[1024];
int length;
while ((length = inputStream.read(buffer)) != -1) {
    result.write(buffer, 0, length);
}
// StandardCharsets.UTF_8.name() > JDK 7
return result.toString("UTF-8");
```

- Using BufferedReader (`JDK`). Warning: This solution convert different line breaks (like `\n\r`) to `line.separator` system property (for example, in Windows to "\r\n").

``` java
String newLine = System.getProperty("line.separator");
BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
StringBuilder result = new StringBuilder();
String line; boolean flag = false;
while ((line = reader.readLine()) != null) {
    result.append(flag? newLine: "").append(line);
    flag = true;
}
return result.toString();
```

- Using BufferedInputStream and ByteArrayOutputStream (`JDK`)

``` java
BufferedInputStream bis = new BufferedInputStream(inputStream);
ByteArrayOutputStream buf = new ByteArrayOutputStream();
int result = bis.read();
while(result != -1) {
    buf.write((byte) result);
    result = bis.read();
}
// StandardCharsets.UTF_8.name() > JDK 7
return buf.toString("UTF-8");
```

- Using inputStream.read() and StringBuilder (`JDK`). Warning: This solution has problem with Unicode, for example with Russian text (work correctly only with non-Unicode text)

``` java
int ch;
StringBuilder sb = new StringBuilder();
while((ch = inputStream.read()) != -1)
    sb.append((char)ch);
return sb.toString();
```


### Warning

1. Solutions `4`, `5` and `9` convert different line breaks to one.

2. Solution `11` can't work correctly with Unicode text

### Performance tests

Performance tests for small `String` (length = 175), url in github (mode = Average Time, system = Linux, score 1,343 is the best):

	              Benchmark                         Mode  Cnt   Score   Error  Units
	 8. ByteArrayOutputStream and read (JDK)        avgt   10   1,343 ± 0,028  us/op
	 6. InputStreamReader and StringBuilder (JDK)   avgt   10   6,980 ± 0,404  us/op
	10. BufferedInputStream, ByteArrayOutputStream  avgt   10   7,437 ± 0,735  us/op
	11. InputStream.read() and StringBuilder (JDK)  avgt   10   8,977 ± 0,328  us/op
	 7. StringWriter and IOUtils.copy (Apache)      avgt   10  10,613 ± 0,599  us/op
	 1. IOUtils.toString (Apache Utils)             avgt   10  10,605 ± 0,527  us/op
	 3. Scanner (JDK)                               avgt   10  12,083 ± 0,293  us/op
	 2. CharStreams (guava)                         avgt   10  12,999 ± 0,514  us/op
	 4. Stream Api (Java 8)                         avgt   10  15,811 ± 0,605  us/op
	 9. BufferedReader (JDK)                        avgt   10  16,038 ± 0,711  us/op
	 5. parallel Stream Api (Java 8)                avgt   10  21,544 ± 0,583  us/op

Performance tests for big `String` (length = 50100), url in github (mode = Average Time, system = Linux, score 200,715 is the best):

	               Benchmark                        Mode  Cnt   Score        Error  Units
	 8. ByteArrayOutputStream and read (JDK)        avgt   10   200,715 ±   18,103  us/op
	 1. IOUtils.toString (Apache Utils)             avgt   10   300,019 ±    8,751  us/op
	 6. InputStreamReader and StringBuilder (JDK)   avgt   10   347,616 ±  130,348  us/op
	 7. StringWriter and IOUtils.copy (Apache)      avgt   10   352,791 ±  105,337  us/op
	 2. CharStreams (guava)                         avgt   10   420,137 ±   59,877  us/op
	 9. BufferedReader (JDK)                        avgt   10   632,028 ±   17,002  us/op
	 5. parallel Stream Api (Java 8)                avgt   10   662,999 ±   46,199  us/op
	 4. Stream Api (Java 8)                         avgt   10   701,269 ±   82,296  us/op
	10. BufferedInputStream, ByteArrayOutputStream  avgt   10   740,837 ±    5,613  us/op
	 3. Scanner (JDK)                               avgt   10   751,417 ±   62,026  us/op
	11. InputStream.read() and StringBuilder (JDK)  avgt   10  2919,350 ± 1101,942  us/op

Performance test (Average Time) depending on Input Stream length in Windows 7 system:

	 length  182    546     1092    3276    9828    29484   58968
	
	 test8  0.38    0.938   1.868   4.448   13.412  36.459  72.708
	 test4  2.362   3.609   5.573   12.769  40.74   81.415  159.864
	 test5  3.881   5.075   6.904   14.123  50.258  129.937 166.162
	 test9  2.237   3.493   5.422   11.977  45.98   89.336  177.39
	 test6  1.261   2.12    4.38    10.698  31.821  86.106  186.636
	 test7  1.601   2.391   3.646   8.367   38.196  110.221 211.016
	 test1  1.529   2.381   3.527   8.411   40.551  105.16  212.573
	 test3  3.035   3.934   8.606   20.858  61.571  118.744 235.428
	 test2  3.136   6.238   10.508  33.48   43.532  118.044 239.481
	 test10 1.593   4.736   7.527   20.557  59.856  162.907 323.147
	 test11 3.913   11.506  23.26   68.644  207.591 600.444 1211.545

### Notes

After you manipulate the InputStream first time, the "cursor" position will be moved to the last of the InputStream. As a result, if you try to manipulate it again, the result will be incorrect. 
 
If you need to manipulate an InputStream multiple times, you have to run the below codes to reset/reopen it.

``` java
InputStream inputStream = new FileInputStream("in.dat");

// first processing
String result = new BufferedReader(new InputStreamReader(inputStream))
		  .lines().collect(Collectors.joining("\n"));

System.out.println(result);

// the below code will be required to manipulate FileInputStream multiple times
inputStream.close();
inputStream = new FileInputStream("in.dat");

// the below code will be required to manipulate ByteArrayInputStream multiple times
//inputStream.reset();

//second processing
String result2 = new BufferedReader(new InputStreamReader(inputStream))
		  .lines().collect(Collectors.joining("\n"));

System.out.println(result2);
```
