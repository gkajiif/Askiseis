structure S = BinaryMapFn(struct
	type ord_key = int
	val compare = Int.compare
end)

fun parse file =
let
    fun next_String input = (TextIO.inputAll input) 
    val stream = TextIO.openIn file
    val a = next_String stream
in
    explode(a)
end 

fun AND arg1 arg2 =
  if arg1 = true then if arg2 = true then true else false else false 
(*
fun NOT_EQ arg1 arg2 =
  if arg1 <> arg2 then true else false
*)
fun calculateWidth listName count = if hd(listName) = #"\n" then count else calculateWidth (tl(listName)) (count+1) (* Start with calculateWidth listName 0 *)


fun calculateHeight listName rowSize =(length(listName)) div (rowSize+1);  (*Used to calculate the height of the map based on the mapInLine*)

fun checkBounds x y width height= (*Checks whether x,y is within the bounds of the Map *)
	(AND (let val check = if x<width then true else false in check end)
		(AND (let val check = if y<height then true else false in check end)  
			(AND (let val check = if x>=0 then true else false in check end) (let val check = if y>=0 then true else false in check end)
				)))
				

fun getPos x y width dataMap= (*Function for acquiring an object from the map based on coordinates x,y *) 
let
	val position = x + y*width
	val temp = S.find(dataMap,position)
in
	if isSome(temp) then valOf(temp) else 0
end

fun setPos x y width value dataMap = (*Function for changing the value of an specific tile in the map *) 
let 
	val position = x +y*width
in
	(S.insert(dataMap,position,value))
end

fun doVisit x y width dataMap = 
	if (getPos x y width dataMap) =0 then setPos x y width (~1) dataMap else setPos x y width ((getPos x y width dataMap)*(~1))  dataMap

fun letterToMap listName dataMap curPos waterCount WaterPositions count catPos= (*Function for placing the numbers within the ORD_MAP ~ Also counts water and the pos*)
	case (hd(listName)) of 
	(#"A") => if (tl(listName)) <>[] then letterToMap (tl(listName)) (S.insert(dataMap,curPos,0)) (curPos+1) waterCount WaterPositions (count+1) (count) else ((S.insert(dataMap,curPos,0)),waterCount,WaterPositions,count)|
	(#"X") => if (tl(listName)) <>[] then letterToMap (tl(listName)) (S.insert(dataMap,curPos,1)) (curPos+1) waterCount WaterPositions (count+1) catPos else ((S.insert(dataMap,curPos,1)),waterCount,WaterPositions,catPos)|
	(#"W") => if (tl(listName)) <>[] then letterToMap (tl(listName)) (S.insert(dataMap,curPos,2)) (curPos+1) (waterCount+1)(WaterPositions @ [count]) (count +1) catPos	else ((S.insert(dataMap,curPos,2)),waterCount+1,(WaterPositions @ [count]),catPos)|
	(#".") => if (tl(listName)) <>[] then letterToMap (tl(listName)) (S.insert(dataMap,curPos,3)) (curPos+1) waterCount WaterPositions (count+1) catPos else ((S.insert(dataMap,curPos,3)),waterCount,WaterPositions,catPos)|
	(#"\n")=> if (tl(listName)) <>[] then letterToMap (tl(listName)) dataMap curPos waterCount WaterPositions count catPos else (dataMap,waterCount,WaterPositions,catPos)|
	 _ => if (tl(listName)) <>[] then letterToMap (tl(listName)) dataMap curPos waterCount WaterPositions count catPos else (dataMap,waterCount,WaterPositions,catPos)

fun bfsWater x y time dataMap width height = 
	if (getPos x y width dataMap) >= 4  andalso (getPos x y width dataMap) <= time then dataMap (*return doing nothing*)  
		else
(*		
			if time <> 3 then (* val nDataMap = (setPos x y width time dataMap) *)
			*)
			let 
				val map = if (checkBounds x (y-1) width height) andalso (((getPos x (y-1) width (setPos x y width time dataMap)) <> 1) andalso ((getPos x (y-1) width (setPos x y width time dataMap))<>2))
					then bfsWater x (y-1) (time+1)(setPos x y width time dataMap) width height else (setPos x y width time dataMap)
				val map = if (checkBounds (x-1) y width height) andalso ((getPos (x-1) y width map)<>1 andalso ((getPos (x-1) y width map)<>2))
					then bfsWater (x-1) y (time+1) map width height else map
				val map = if  (checkBounds x (y+1) width height) andalso ((getPos x (y+1) width map)<>1 andalso ((getPos x (y+1) width map)<>2))
					then bfsWater x (y+1) (time+1) map width height else map
				val map = if (checkBounds (x+1) y width height) andalso ((getPos (x+1) y width map)<>1 andalso ((getPos (x+1) y width map)<>2))
					then bfsWater (x+1) y (time+1) map width height else map		
			in
				map
			end
			(*
			else  (*val nDatamap = dataMap*)
			let 
				val map = if (checkBounds x (y-1) width height) andalso (((getPos x (y-1) width dataMap) <> 1) andalso ((getPos x (y-1) width dataMap)<>2))
					then bfsWater x (y-1) (time+1) dataMap width height else dataMap
				val map = if (checkBounds (x-1) y width height) andalso ((getPos (x-1) y width map)<>1 andalso ((getPos (x-1) y width map)<>2))
					then bfsWater (x-1) y (time+1) map width height else map
				val map = if (checkBounds x (y+1) width height) andalso ((getPos x (y+1) width map)<>1 andalso ((getPos x (y+1) width map)<>2))
					then bfsWater x (y+1) (time+1) map width height else map
				val map = if (checkBounds (x+1) y width height) andalso ((getPos (x+1) y width map)<>1 andalso ((getPos (x+1) y width map)<>2))
					then bfsWater (x+1) y (time+1) map width height else map
			in
				map
			end
			*)
	
fun best_pos_first dataMap x y width=  (*curMove is the list of characters containing the moves, moveList is the moves_res *)
	if (getPos x y width dataMap) = 1 then 
		if (getPos x y width dataMap) < 0 then false 
		else
			true
			
			
	else 
	false
	
fun best_pos_mid dataMap x y width placeZero placeOne curMov moveList rCount =
	if ((x+y) < (placeZero + placeOne) ) orelse ((x+y) = (placeOne + placeZero) andalso (y < placeZero)) then
	(*val placeZero = y*)
	(*val placeOne =x*)
	(*S.insert(moveList,curMov)*)
	(*val rCount = rCount+1 *)
	(y,x,(moveList @ [curMov]),(rCount+1))
	else (placeZero,placeOne,moveList,rCount)

fun best_pos (placeZero,placeOne,moveList,rCount) curMov dataMap x y width height =
if (best_pos_first dataMap x y width) then 
let
	(* #1 ppmc = new place[0] , #2 ppmc = new place[1] , #3 ppmc = moveList, #4 ppmc = rCount *)
	val ppmc = (best_pos_mid dataMap x y width placeZero placeOne curMov moveList rCount)
	val bp = if(x-1) >= 0 then (best_pos ppmc (curMov @ [#"L"]) (doVisit x y width dataMap) (x-1) y width height) else ((doVisit x y width dataMap),ppmc, curMov)
	val bp = if(y-1) >= 0 then (best_pos (#2 bp) (curMov @ [#"U"]) (#1 bp) x (y-1) width height) else bp 
	val bp = if(y+1) < height then (best_pos (#2 bp) (curMov @ [#"D"]) (#1 bp) x (y+1) width height) else bp 
	val bp = if(x+1) < width then (best_pos (#2 bp) (curMov @ [#"R"]) (#1 bp) (x+1) y width height) else bp 
in bp end  
else (dataMap,(placeZero,placeOne,moveList,rCount), curMov)

fun bfsCat  x y time dataMap (result,placeZero,placeOne,rCount,moveList) width height curMove = 
(* I am turning the value negative on the parts where the cat has already visited. *)
if (getPos x y width dataMap) <4 then (result,placeZero,placeOne,rCount,moveList)
else 
	(* doVisit x y dataMap  # m[y][x] gets visited *) 
	if (getPos x y width dataMap) <= time andalso 
		(getPos x y width dataMap) >=4 andalso ( 
		(getPos x y width dataMap) >result orelse ( (getPos x y width dataMap) = result andalso (
		((x+y) < (placeZero +placeOne) )orelse( (x+y) = (placeOne + placeZero) andalso y <placeZero) ))  ) 
		then  ((getPos x y width dataMap), y,x,(rCount +1),(moveList @ [curMove]))
		
	else 
		let 
			val aCondition = if (getPos x y width dataMap) >=4 andalso ( 
			(getPos x y width dataMap) >result orelse ( (getPos x y width dataMap) = result andalso (
			((x+y) < (placeZero +placeOne) )orelse( (x+y) = (placeOne + placeZero) andalso y <placeZero) ))  ) 
			then true else false 
			val pZ = if aCondition then  y else placeZero
			val pO = if aCondition then x else placeOne
			val res = if aCondition then (getPos x y width dataMap) else result
			val rC = if aCondition then (rCount +1) else rCount
			val mvL = if aCondition then (moveList @ [curMove]) else moveList
			
			(*
			val someTuple = if (y-1) >=0 andalso (getPos x (y-1) width dataMap) <>1 then bfsCat x (y-1) (time +1) (doVisit x y width dataMap) width height res pZ pO rC mvL (curMove @ [#"U"]) else ((doVisit x y width dataMap),res,pZ,pO,rC,mvL) (*I GUESSS SOMETHING WITH NO CHANGES*)
			val someTuple = if (x-1) >=0 andalso (getPos (x-1) y width dataMap) <>1 then bfsCat (x-1) y (time +1) (#1 someTuple) width height (#2 someTuple) (#3 someTuple) (#4 someTuple) (#5 someTuple) (#6 someTuple) (curMove @ [#"L"]) else someTuple(*I GUESSS SOMETHING WITH NO CHANGES*)
			val someTuple = if (y+1) >=0 andalso (getPos x (y+1) width dataMap) <>1 then bfsCat x (y+1) (time +1) (#1 someTuple) width height (#2 someTuple) (#3 someTuple) (#4 someTuple) (#5 someTuple) (#6 someTuple) (curMove @ [#"D"]) else someTuple(*I GUESSS SOMETHING WITH NO CHANGES*)
			val someTuple = if (x+1) >=0 andalso (getPos (x+1) y width dataMap) <>1 then bfsCat (x+1) y (time +1) (#1 someTuple) width height (#2 someTuple) (#3 someTuple) (#4 someTuple) (#5 someTuple) (#6 someTuple) (curMove @ [#"R"]) else someTuple(*I GUESSS SOMETHING WITH NO CHANGES*)
			*)
		in
			if (y-1) >=0 then
				if (x-1) >=0 then 
					if (y+1) <height then
						if (x+1) <width then (*ULDR*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y+1) (time +1) (doVisit x y width dataMap) ( bfsCat (x-1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])
										) width height (curMove @ [#"L"])
									) width height (curMove @ [#"D"])
								) width height (curMove @ [#"R"])
								(*ULD*)
						else bfsCat x (y+1) (time +1) (doVisit x y width dataMap) ( bfsCat (x-1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])
										) width height (curMove @ [#"L"])
									) width height (curMove @ [#"D"])	
					else
						if (x+1) < width  then (*ULR*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap) ( bfsCat (x-1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])
										) width height (curMove @ [#"L"])
								) width height (curMove @ [#"R"])
								(*UL*)
						else bfsCat (x-1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])
										) width height (curMove @ [#"L"])
				
				else 
					if (y+1) <height then
						if (x+1) < width then
						(*UDR*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y+1) (time +1) (doVisit x y width dataMap)(bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])
									) width height (curMove @ [#"D"])
								) width height (curMove @ [#"R"])
								(*UD*)
						else bfsCat x (y+1) (time +1) (doVisit x y width dataMap) (bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])
									) width height (curMove @ [#"D"])	
					else
						if (x+1) <width then (*UR*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])
								) width height (curMove @ [#"R"])
								(*U*)
						else bfsCat x (y-1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											width height (curMove @ [#"U"])					
			else
				if (x-1) >=0 then 
					if (y+1) <height then
						if (x+1) <width then (*LDR*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y+1) (time +1) (doVisit x y width dataMap) ( bfsCat (x-1) y (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											
										 width height (curMove @ [#"L"])
									) width height (curMove @ [#"D"])
								) width height (curMove @ [#"R"])
								(*LD*)
						else bfsCat x (y+1) (time +1) (doVisit x y width dataMap) ( bfsCat (x-1) y (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											
										 width height (curMove @ [#"L"])
									 )width height (curMove @ [#"D"])	
					else
						if (x+1) <width then (*LR*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap) ( bfsCat (x-1) y (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											
										 width height (curMove @ [#"L"])
								) width height (curMove @ [#"R"])
								(*L*)
						else bfsCat (x-1) y (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)	
										 width height (curMove @ [#"L"])
				
				else 
					if (y+1) <height then
						if (x+1) <width then (*DR*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap) (bfsCat x (y+1) (time +1)
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											
									 width height (curMove @ [#"D"])
								) width height (curMove @ [#"R"])
								(*D*)
						else bfsCat x (y+1) (time +1) 
											(doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
											
									 width height (curMove @ [#"D"])	
					else
						if (x+1) <width then (*R*)
							bfsCat (x+1) y (time +1) (doVisit x y width dataMap)(res,pZ,pO,rC,mvL)
										width height (curMove @ [#"R"])
						else  (res,pZ,pO,rC,mvL)
											(*(doVisit x y width dataMap)*)
		
	end


fun savethecat file =
let
	val mapInLine = (parse file)
	val aTuple = letterToMap mapInLine S.empty 0 0 [] 0 (~1) (*#1 aTuple: ordMap, #2 aTuple: total number of initial Water, #3 aTuple: water positions, #4 aTuple: Cat Position*)
	val width = calculateWidth mapInLine 0
	val height = calculateHeight mapInLine width
	
	fun burstPipes 0 pipesPos width height floodMap= floodMap
	|   burstPipes num pipesPos width height floodMap = burstPipes (num-1) (tl(pipesPos)) width height (bfsWater (hd(pipesPos) mod width) (hd(pipesPos) div width) (3) floodMap width height)
	
	val map = burstPipes (#2 aTuple) (#3 aTuple) width height (#1 aTuple)
	
in
	
	if (getPos (#4 aTuple mod width) (#4 aTuple div width) width map) =0 then 
		let 
			val omgAMAZING = (best_pos (((#4 aTuple) div width),((#4 aTuple) mod width),[],0) [] map ((#4 aTuple) mod width) ((#4 aTuple) div width) width height)

		in
		
			if (length(#3(#2 omgAMAZING)))= 0 orelse (null(hd(rev(#3(#2 omgAMAZING))))) then print("infinity\nstay\n") else print("infinity\n"^(implode(hd(rev(#3(#2 omgAMAZING)))))^"\n")
		
		
		  
		end
	else
		let 
			val omgSUPER = (bfsCat (#4 aTuple mod width) (#4 aTuple div width) 3 map (4,(#4 aTuple div width),(#4 aTuple mod width),0,[]) width height [])
		in
		
			if (null(#5 omgSUPER)) orelse (length(hd(rev(#5 omgSUPER)))) =0 then print(Int.toString((#1 omgSUPER)-4)^"\nstay\n") else 
			print(Int.toString((#1 omgSUPER)-4)^"\n"^(implode(hd(rev(#5 omgSUPER))))^"\n")
		
			
			
		end
		
end

(* COMMENT SECTION FOR COPY PASTING DEBUG STUFF
 print (Int.toString(valOf(S.find(map,1))))
 
 #3(#2 omgAMAZING)
 
 #5 omgSUPER
*)