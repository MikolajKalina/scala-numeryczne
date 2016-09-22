def readLimes():(Double,Double) = {
 println("Podaj dolna granice :")
 val a = CheckedInput
 println("Podaj gorna granice :")
 val b = CheckedInput
 (a,b)
}

def CheckedInput:Double ={
try{
 readDouble
} catch{
   case e:NumberFormatException => println("Prosze podac wartosci liczbowe")
   CheckedInput
 }
}

def getFuncval(x:Double): Double = (1/x)

def trapezMethod(tuple:(Double,Double)): Double = (getFuncval(tuple._1) + getFuncval(tuple._2))/2

def secondTrapezMethod(tuple:(Double,Double) ): Double = {
var pivot = (tuple._1 + tuple._2)/2
(getFuncval(tuple._1)/2 + getFuncval(tuple._2)/2 + getFuncval(pivot))/2
}

def thirdTrapezMethod(tuple:(Double,Double)): Double = {
var pivot = (tuple._1 + tuple._2)/2
var quater = (tuple._1+pivot)/2
var threequater = (pivot + tuple._2)/2

((getFuncval(tuple._1)/2 + getFuncval(tuple._2)/2 + getFuncval(pivot))/4) +
((getFuncval(quater) + getFuncval(threequater))/4)
}

def main():Unit = {
var limes = readLimes()
println("Granica od : " +limes._1 + " do : "+limes._2)
var x1 = trapezMethod(limes)
var x2 = secondTrapezMethod(limes)
var x3 = thirdTrapezMethod(limes)

if((x1>x2) && (x2>x3)){
println("Calka (jej przyblizenie) wynosi :" + ((4*thirdTrapezMethod(limes) - secondTrapezMethod(limes))/3))
}
else println("Ciąg nie maleje monotonicznie - zalozenia Romberga nie sa spenione\nNie da sie ta metoda oszacowac tej calki")
}
//---------------------------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------------------------------------
var secondLimes = readLimes()

def SecondTrapezMethod(x:Int):Double = {
var NofDiv = x
var sum = 0.0
var dx= (secondLimes._2-secondLimes._1)/NofDiv

for(i<- 1 to NofDiv-1) sum += getFuncval(secondLimes._1+i*dx)
sum+=(getFuncval(secondLimes._1)/2)+(getFuncval(secondLimes._1+NofDiv*dx)/2)
sum*dx
}


def TwoPower(i:Int):Int = {
var base = 1
var x=0

do{
 base*=2
 x+=1
}while(x<i)
base
} 

def FourPower(i:Int):Int ={
var base = 1
var x=0

do{
 base*=4
 x+=1
}while(x<i)
base
} 

	//zamienic acc na length
def RombergEquation(tab:Array[Array[Double]],accuracy:Int):Double ={

	for( i<- 1 to accuracy-1; j<-0+i  to accuracy-1){
		tab(i)(j)= (tab(i-1)(j)*FourPower(i) - tab(i-1)(j-1))/(FourPower(i)-1)
	}
	
	//Odwrocona kolejnosc by wygladala jak na wykladach
	for( i<- 0 to accuracy-1 ; j<- 0 to accuracy-1) {
	print("     "+tab(j)(i))	
	if(j==accuracy-1)println()
	}

	0.0	
}

def ExtendedRomberg():Double ={
println("Prosze podac ilosc wierszy (im wiecej tym wieksza dokladnosc) :")
var accuracy = readInt
var tab = Array.ofDim[Double](accuracy,accuracy)

for(i <- 0 to accuracy-1) tab(0)(i)=SecondTrapezMethod(TwoPower(i+1))
RombergEquation(tab,accuracy)
}

ExtendedRomberg()

