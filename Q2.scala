import scala.io.StdIn._

object Q2{
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be between 0 and total possible marks"))
    } else {
      (true, None)
    }
  }

  def getStudentInfo: (String, Int, Int, Double, Char) = {
    println("Enter student name:")
    val name = readLine()

    println("Enter marks obtained:")
    val marks = readInt()

    println("Enter total possible marks:")
    val totalMarks = readInt()

    val percentage = (marks.toDouble / totalMarks) * 100

    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }

    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  def getStudentInfoWithRetry: (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentRecord: (String, Int, Int, Double, Char) = null

    while (!isValid) {
      println("Enter student name:")
      val name = readLine()

      println("Enter marks obtained:")
      val marks = readInt()

      println("Enter total possible marks:")
      val totalMarks = readInt()

      val (valid, errorMessage) = validateInput(name, marks, totalMarks)
      if (valid) {
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        studentRecord = (name, marks, totalMarks, percentage, grade)
        isValid = true
      } else {
        println(s"Invalid input: ${errorMessage.get}")
      }
    }

    studentRecord
  }

  def main(args: Array[String]): Unit = {
    val studentRecord = getStudentInfoWithRetry
    printStudentRecord(studentRecord)
  }
}
