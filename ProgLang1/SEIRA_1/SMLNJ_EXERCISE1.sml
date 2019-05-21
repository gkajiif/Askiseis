structure S = BinaryMapFn(struct
	type ord_key = int
	val compare = Int.compare
end)

fun colors file =
    let
	(* A function to read an integer from specified input. *)
        fun readInt input = 
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);

	(* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (size of the Ribbon) *)
		val n = readInt inStream
	
		(*Read number of colours and consume newline*)
		val k = readInt inStream
		val _ = TextIO.inputLine inStream

        (* A function to read N integers from the open file. *)
		fun readInts 0 acc = acc (* Replace with 'rev acc' for proper order. *)
		| readInts i acc = readInts (i - 1) (readInt inStream :: acc)
	
		
	
		val colorTable = S.empty
		fun initColAdvanced start finish map = 
			let 
				val mid = (start+finish) div 2
				val map = if isSome(S.find(map,mid)) then map else S.insert(map,mid,0)
				val map = if mid +1 >= finish then S.insert(map,mid+1,0) else initColAdvanced (mid+1) finish map
				val map = if mid -1 <= start then S.insert(map,mid-1,0) else initColAdvanced start (mid-1) map
			in 
			map
			end 
		fun minimum value0 value1 = if (value0 > value1) then value1 else value0

		(* Beginning the Algorithm *)
		val whatToOutput = n + 1
		val totalForTrue = k 
		fun solve held ribbon currSize colorMap curMin colorsCount sumToEnd checkeroo=   
			let 
				val preValue = 
					if null(ribbon) then 0 
					else 
						let 
							val temp = S.find(colorMap,hd(ribbon)) 
						in 
							if(isSome(temp)) then valOf(temp) +1 else 0 
						end
				val newSum = if preValue = 1 then sumToEnd+1 else sumToEnd (*Checks if it was the first time a color was added and adds that color to the checksum*)
				val nHeld = if null(ribbon) then held else held @ [hd(ribbon)] (*Adds the head of the second list as the tail to the first list *)
				val nRibbonLeft = if null(ribbon) then ribbon else tl(ribbon) (* Drops the first element of the remaining ribbon*)
				val currSize = currSize + 1
				fun dropHeadIf listName colorM cSize=  (*Checks if the head color exists more than once, if so drops it. Returns size*)
					let 
						val checking = hd(listName)
						val check = valOf(S.find(colorM,checking))
						
			
					in
						if  check > 1 then dropHeadIf (tl(listName)) (S.insert(colorM,checking,(check-1))) (cSize-1)
						else (cSize,colorM,listName)
					end 
				val temporaryTuple = if null(ribbon) then (currSize,(S.insert(colorMap,hd(rev(nHeld)),preValue)),nHeld) else 
						if hd(nHeld) = hd(rev(nHeld)) then dropHeadIf nHeld (S.insert(colorMap,hd(nHeld),preValue)) currSize else 
						(currSize,(S.insert(colorMap,hd(rev(nHeld)),preValue)),nHeld)
				val nCurrSize = #1 temporaryTuple
				val colorMap = #2 temporaryTuple
				val nHeld = #3 temporaryTuple
				val curMinTRUE = minimum curMin nCurrSize
			in 
				if (newSum = checkeroo) then 
					if null(ribbon) then curMinTRUE
					else solve nHeld nRibbonLeft nCurrSize (#2 temporaryTuple) curMinTRUE colorsCount newSum checkeroo
				else 
					if null(ribbon)  then curMin
					else solve nHeld nRibbonLeft nCurrSize (#2 temporaryTuple) curMin colorsCount newSum checkeroo
			end
		val whatToOutput = solve nil (readInts n []) 0 (initColAdvanced 0 k colorTable) whatToOutput k 0 totalForTrue
	
    in
		(*n: Is the size of the ribbon, k: The number of colours, ribbon: the List containing all numbers *)
		if whatToOutput = n +1 then print("0\n") else print(Int.toString(whatToOutput)^"\n") (*Gives the output*) 
    end