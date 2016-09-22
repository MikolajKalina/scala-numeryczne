import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import scala.collection.mutable.ListBuffer
import scala.io.Source
//-----------------------------------------------------------------------------------
def loadData: List[Double] = {

 val text: File = new File("matrix.txt")
 val scanner : Scanner = new Scanner(text)
 val ArrBuff = scala.collection.mutable.ArrayBuffer.empty[Double]
 while(scanner.hasNext){
  ArrBuff+= scanner.nextDouble

}
 ArrBuff.toList
}
//-----------------------------------------------------------------------------------
def getNumLines : Int = Source.fromFile("matrix.txt").getLines.length+1
//-----------------------------------------------------------------------------------

def splitList(list:List[Double],n:Int) : List[List[Double]] ={
  def splitInner(res:List[List[Double]], lst:List[Double], n:Int) : List[List[Double]] = {
	if(lst.isEmpty) res
	else{
	 val headList:List[Double] = lst.take(n)
	 val tailList:List[Double] = lst.drop(n)
	 splitInner(headList :: res, tailList, n)
	}
  }
 splitInner(Nil, list, n).reverse
}
//-----------------------------------------------------------------------------------


def removeFirstUnknown(list: List[Double]) : List[Double] = 
if(list(0) == 0.0) 0.0 :: removeFirstUnknown(list.tail)
else list.map(l => l / list(0) )   

//-----------------------------------------------------------------------------------

def multiplyLine(list: List[Double], number: Double): List[Double] = list.map(l=> l*number)

//-----------------------------------------------------------------------------------

def SubLists(list1: List[Double], list2: List[Double]): List[Double] = {
 val tmp2 = getFirstElement(list2)
 val tmp1 = getFirstElement(list1)
 val ArrBuff = scala.collection.mutable.ArrayBuffer.empty[Double]
 for(i <- 0 to list2.length-1) ArrBuff += (list2(i)*tmp1 - (list1(i) * tmp2)) 
 ArrBuff.toList
}

//-----------------------------------------------------------------------------------

def getFirstElement(list: List[Double]) : Double = {
  if(list(0) == 0.0) getFirstElement(list.tail)
  else list(0)
}
//-----------------------------------------------------------------------------------

def ZeroCol(list: List[Double], lists: List[List[Double]], flag:Int): List[List[Double]] = {
val ArrBuff = scala.collection.mutable.ArrayBuffer.empty[List[Double]]
if(flag==0){
      ArrBuff+=removeFirstUnknown(list)
	for( i<- 1 to lists.length-1)
	if(lists(i)!=0.0) ArrBuff+=SubLists(removeFirstUnknown(list),lists(i))
	} else {
          for( i<- 0 to flag-1) ArrBuff+=lists(i)
 	   ArrBuff+=removeFirstUnknown(lists(flag))
	   ArrBuff+=SubLists(removeFirstUnknown(lists(flag)),lists(flag+1))
           
        }
ArrBuff.toList
}
//-----------------------------------------------------------------------------------
def Solve(lists: List[Double], tab : List[Double]) : List[Double]={
val ArrBuff = scala.collection.mutable.ArrayBuffer.empty[Double]

for(i<-0 to lists.length-1)
 ArrBuff+= lists(i) * tab(i) / lists(lists.length-1)

 
 ArrBuff.toList 
}
//-----------------------------------------------------------------------------------
def mainLoop: List[List[Double]] ={
var lista = splitList(loadData,getNumLines)
val tmp = lista.length-2

for(i<-0 to tmp){
lista = ZeroCol(lista(0), lista,i)
println("\n" + lista+ "\n")
}
lista
}
//-----------------------------------------------------------------------------------
println(loadData)
val x =mainLoop
println(x.reverse)


//-----------------------------------------------------------------------------------
/*println(loadData)
val lists = splitList(loadData, 4)
println(removeFirstUnknown(lists(1)))
println(getFirstElement(lists(2)))
println(multiplyLine(lists(0) , 4))
println(SubLists(lists(0),lists(1)))*/


/*

//def mainLoop(list:List[List[Double]]): List[List[Double]] ={
 val ListBuff = scala.collection.mutable.ArrayBuffer.empty[List[Double]]
 ArrBuff += ZeroC
lista = ZeroCol(lista(0),lista)
println(lista)
ListBuff +=lista
lista = ZeroCol(lista(0),lista)
ListBuff +=lista

println(ListBuff)


//def mainLoop(list:List[List[Double]]): List[List[Double]] ={
 val ArrBuff = scala.collection.mutable.ArrayBuffer.empty[List[List[Double]]]
 ArrBuff += ZeroCol(lista(0),lista)
 ArrBuff += ZeroCol(lista(1),lista)


println(ArrBuff.toList)

*/

