package main
import "fmt"
import "container/list"


// takes in a list and a positive integer N
// needs to return a list:  every Nth element of L
func everyNth(L *list.List, N int) *list.List {
	/* psuedocode: 
		have a position variable that keeps track of what index you are on the list
		since we only want to insert the Nth element into the new list, we just mod our current position with N
		if that operation == 0, then insert that index into the new list
		return the new list
	*/

	listPos := 0
	l := list.New()
	for i := L.Front(); i != nil ; i = i.Next() {
    	//increment the position var
    	listPos += 1

    	//mod logic
    	if listPos % N == 0{
    		//i.Value = l[i]
    		l.PushBack(i.Value)
    	}

	}
	return l

}


//driver function
func main(){
	testList := list.New()
	//create a list of {1, 2, 3...., 1000}
	for i := 0; i < 1000; i++ {
		testList.PushBack(i)
	}

	outputList := everyNth(testList, 109)
	for i := outputList.Front(); i != nil ; i = i.Next() {
		fmt.Println(i.Value)
	}

}



