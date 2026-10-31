# ALSM Cobol2Java Batch Converter (AKAMIG-Batch)

Hệ thống chuyển đổi mã nguồn COBOL sang mã nguồn Java Batch chuẩn cho dự án **ALSM** (Automated Legacy System Modernization).

---

## 1. Tổng quan kiến trúc (Architecture)

Công cụ này dịch trực tiếp các chương trình COBOL Batch (`.cob`, `.cbl`) và các Copybook đi kèm sang các lớp Java:
- Lớp xử lý nghiệp vụ chương trình (Program class, ví dụ: `Model01.java`).
- Lớp cấu trúc dữ liệu mô hình bản ghi (Record/Data Model class, ví dụ: `Model01Model.java`).
- Các lớp hỗ trợ truy xuất file, xử lý byte mảng, binary COMP/COMP-3, so sánh EBCDIC/ASCII, định dạng trường số học.

Mã nguồn Java sinh ra từ công cụ này tương thích hoàn toàn với thư viện runtime chuẩn của ALSM:
- **`Batch`** (`d:\ALSM\Batch\Batch`): Khung thực thi Spring Batch Runner cho các batch jobs.
- **`Common`** (`d:\ALSM\Common\Common`): Thư viện lớp cơ sở (`jp.co.alsm.common.*`).

Package mặc định khi sinh mã nguồn được cấu hình trong `Config.properties`:
```properties
programPackage = src\main\java\jp\co\alsm\batch
dataPackage = src\main\java\jp\co\alsm\batch
```

---

## 2. Yêu cầu môi trường (Prerequisites)

- **Java Development Kit (JDK)**: Java 8 hoặc Java 17/21 (khuyến nghị JDK 21 LTS).
- **Apache Maven**: Phiên bản 3.6 trở lên.

---

## 3. Hướng dẫn đóng gói (Build)

Để đóng gói ra file JAR thực thi:

```bash
mvn clean package -DskipTests
```

File JAR sau khi build thành công sẽ nằm tại:
```
target/akaBatch-1.0.jar
```

---

## 4. Cách sử dụng (Usage)

### 4.1. Cấu hình chuyển đổi (`Config.properties`)
Đặt file `Config.properties` tại thư mục gốc chạy lệnh hoặc cùng thư mục với file JAR:
```properties
programPackage = src\main\java\jp\co\alsm\batch
dataPackage = src\main\java\jp\co\alsm\batch
doListDir = false
printCobolStatementsAsComments = false
retainCobolComments = true
useNativeJavaTypes = false
useRecursiveJavaMethod = false
isEbcdicMachine = false
isFixedFormat = true
dataNameFormat = 0
tabLength = 4
exceptionPrintStackTraceOn = true
sqlDatabaseType = 1
```

### 4.2. Chạy lệnh chuyển đổi
```bash
java -jar target/akaBatch-1.0.jar <đường_dẫn_file_cobol_hoặc_thư_mục>
```

### 4.3. Tích hợp trong ALSM Backend
Trong file cấu hình `.env` của `ALSM_WEB_BE`:
```env
TOOL2JAVA_JAR_PATH=D:\ALSM\tool2java\target\akaBatch-1.0.jar
```

---

## 5. Cấu trúc thư mục repository

```
.
├── cobol/              # Thư mục mã nguồn COBOL mẫu và copybooks
├── benchmark/          # Các tệp mẫu kiểm thử hiệu năng và so sánh
├── src/                # Toàn bộ mã nguồn bộ parser & translator COBOL sang Java
│   └── main/
│       ├── java/       # Core translation engine & AST generator
│       └── resources/  # Resource cấu hình
├── Config.properties   # Cấu hình tham số sinh code Java (package, format, sql)
├── Batch.txt           # Danh sách batch jobs mẫu
├── pom.xml             # Maven project configuration
├── .gitignore          # Cấu hình bỏ qua các file tạm/target/IDE
└── README.md           # Tài liệu hướng dẫn
```
