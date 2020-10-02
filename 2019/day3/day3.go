package day3

import (
	"image"
	"log"
	"math"
	"strconv"
	"strings"
)

type point image.Point
type wirePath map[point][]int
type wire struct {
	start point
	end   point
	path  wirePath
}

func (w *wire) addPoint(p point) {
	pointPath := w.path[w.end]
	w.path[p] = append(w.path[p], pointPath[len(pointPath)-1]+1)
}

func (w *wire) setEndPoint(p point) {
	w.end = p
}

func pathToWire(path string) wire {
	w := wire{
		start: point{0, 0},
		end:   point{0, 0},
		path:  wirePath{point{0, 0}: []int{0}},
	}

	directions := strings.Split(path, ",")
	for _, dir := range directions {
		np := point{0, 0}
		switch dir[:1] {
		case "R":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X + 1, w.end.Y}
				w.addPoint(np)
				w.setEndPoint(np)
			}
		case "L":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X - 1, w.end.Y}
				w.addPoint(np)
				w.setEndPoint(np)
			}
		case "U":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X, w.end.Y + 1}
				w.addPoint(np)
				w.setEndPoint(np)
			}
		case "D":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X, w.end.Y - 1}
				w.addPoint(np)
				w.setEndPoint(np)
			}
		}
	}
	return w
}

func getSteps(dir string) int {
	steps, err := strconv.Atoi(dir[1:])
	if err != nil {
		log.Fatal(err)
	}
	return steps
}

func crossing(w1, w2 wire) []point {
	var cross []point
	for k := range w1.path {
		if k.X == 0 && k.Y == 0 {
			continue
		}
		if _, ok := w2.path[k]; ok {
			cross = append(cross, k)
		}
	}
	return cross
}

func manhattanDistance(p1, p2 string) uint {
	w1 := pathToWire(p1)
	w2 := pathToWire(p2)

	crossPoints := crossing(w1, w2)

	var distance float64 = 0.0
	for _, point := range crossPoints {
		// actually, the formula is |x1 - x2| + |y1 - y2|, but
		// control point is always in {0, 0} point. So the simplified formula is
		newDistance := math.Abs(float64(point.X)) + math.Abs(float64(point.Y))
		if distance == 0.0 {
			distance = math.Max(distance, newDistance)
			continue
		}
		distance = math.Min(distance, newDistance)
	}
	return uint(distance)
}

func minSignalDelay(p1, p2 string) int {
	w1 := pathToWire(p1)
	w2 := pathToWire(p2)

	crossPoints := crossing(w1, w2)
	var intersectionMinSum int = 0
	for _, point := range crossPoints {
		pointPath1 := w1.path[point]
		pointPath2 := w2.path[point]
		tempIntersectionMinSum := takeTheStep(pointPath1) + takeTheStep(pointPath2)
		if intersectionMinSum == 0 && tempIntersectionMinSum != 0 {
			intersectionMinSum = tempIntersectionMinSum
			continue
		}
		if tempIntersectionMinSum < intersectionMinSum {
			intersectionMinSum = tempIntersectionMinSum
		}
	}
	return intersectionMinSum
}

func takeTheStep(pointPath []int) int {
	if len(pointPath) == 1 {
		return pointPath[0]
	}
	return pointPath[1]
}
