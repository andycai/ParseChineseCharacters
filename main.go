package main

import (
	"bufio"
	"encoding/csv"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

var total int = 0
var totalI int = 0
var debug bool = false
var maps = map[string]int{}
var exportTable []byte
var importTable []byte = []byte{}
var exportSrcFileContent map[string]string = map[string]string{}

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func isExist(path string) bool {
	_, err := os.Stat(path)
	if err == nil {
		return true
	}

	return os.IsExist(err)
}

func loopPath(pathStr string, dstFileName string) {
	if dstFileName == "" {
		dstFileName = "zh-tw.txt"
	}

	if isExist(dstFileName) {
		b, err1 := ioutil.ReadFile(dstFileName)

		if err1 != nil {
			panic("can't read file" + dstFileName)
		}

		r := csv.NewReader(strings.NewReader(string(b)))
		content, _ := r.ReadAll()

		l := len(content)
		for i := 0; i < l; i++ {
			exportSrcFileContent[content[i][0]] = content[i][1]
		}
	}

	err := filepath.Walk(pathStr, func(pathStr string, fileInfo os.FileInfo, err error) error {
		if fileInfo == nil {
			return err
		}
		if fileInfo.IsDir() {
			return nil
		}

		ext := path.Ext(fileInfo.Name())

		switch ext {
		case ".php":
			fmt.Println(pathStr)
			total = total + 1

			parseFile(pathStr, dstFileName)

		case ".as":
			fmt.Println(pathStr)
			total = total + 1

			parseFile(pathStr, dstFileName)
		}

		return nil
	})

	// optimization
	// write to the file once after parse all files
	f, err := os.OpenFile(dstFileName, os.O_APPEND|os.O_CREATE, 0644)
	f.Write(exportTable)

	defer func() {
		f.Close()
		exportTable = []byte{}
		exportSrcFileContent = map[string]string{}
		if e, ok := recover().(error); ok {
			fmt.Println("[WARNING]", e)
		}
	}()

	check(err)

	fmt.Println("Total", total)
	fmt.Println("Total items", totalI)
}

func parseFile(filePath string, dstFileName string) {
	b, err := ioutil.ReadFile(filePath)
	if err != nil {
		fmt.Println(err)
	}

	r := bufio.NewReader(strings.NewReader(string(b)))

	var lineNum int = 0

	for err == nil {
		isP := true
		var line []byte
		var item []byte

		for isP {
			line, isP, err = r.ReadLine()
			lineNum = lineNum + 1
			if err != nil && err.Error() != "EOF" {
				panic("Can't read")
			}

			if strings.Contains(string(line), "__") && string(line[:2]) != "//" {

				re, _ := regexp.Compile("__\\(\\s*(\\'|\")(.*?)(\\'|\")\\s*\\)")
				submatch := re.FindSubmatch(line)

				if len(submatch) > 2 {

					if _, eok := exportSrcFileContent[string(submatch[2])]; !eok {
						if debug == true {
							fileI := fmt.Sprintf("line:%d %s", lineNum, filePath)
							item = append(item, []byte(fileI)...)
							item = append(item, []byte(",")...)
							item = append(item, line...)
							item = append(item, []byte(",")...)
						}
						item = append(item, submatch[2]...)
						item = append(item, []byte(",")...)
						item = append(item, submatch[2]...)
						item = append(item, []byte("\n")...)

						keyStr := strconv.QuoteToASCII(string(submatch[2]))

						if _, ok := maps[keyStr]; !ok && string(submatch[2]) != "" {
							maps[keyStr] = 1
							exportTable = append(exportTable, item...)
							totalI = totalI + 1
						}
					}
				}
			}
		}
	}
}

func exportAction(tokens []string) {
	if len(tokens) != 3 {
		fmt.Println("Usage: export sourcePath targetFile")
		return
	}

	srcPath := tokens[1]
	dstFileName := tokens[2]

	loopPath(srcPath, dstFileName)
}

func importAction(tokens []string) {
	if len(tokens) != 3 {
		fmt.Println("Usage: import sourceFile targetFile")
		return
	}

	srcFile := tokens[1]
	dstFile := tokens[2]
	if dstFile == "" {
		dstFile = "zh-lang.php"
	}
	b, err := ioutil.ReadFile(srcFile)
	if err != nil {
		panic("Can't read")
	}
	r := csv.NewReader(strings.NewReader(string(b)))
	content, _ := r.ReadAll()

	l := len(content)
	importTable = append(importTable, []byte("<?php defined('SYSPATH') or die('No direct script access.');\n\n")...)
	importTable = append(importTable, []byte("return array\n(\n")...)
	for i := 0; i < l; i++ {
		importTable = append(importTable, []byte(fmt.Sprintf("\t'%s' => '%s',\n", content[i][0], content[i][1]))...)
	}
	importTable = append(importTable, []byte(");")...)

	f, err1 := os.OpenFile(dstFile, os.O_RDWR|os.O_CREATE, 0644)
	f.Write(importTable)

	defer func() {
		f.Close()
		importTable = []byte{}
		if e, ok := recover().(error); ok {
			fmt.Println("[WARNING]", e)
		}
	}()

	check(err1)

	fmt.Printf("import to file [%s] done!\n", dstFile)
}

func main() {
	fmt.Println(`	
	Enter the following commands to use:
	export sourcePath targetFile -- export the Chinese characters to target, like "export codepath zh-tw.csv"
	import sourceFile targetFile -- import the source file Chinese characters to target, like "import zh-tw.csv zh-tw.php"
	quit | q -- quit the tool
	`)

	r := bufio.NewReader(os.Stdin)

	for {
		fmt.Print("Enter command-> ")

		rawLine, _, _ := r.ReadLine()

		line := string(rawLine)

		if line == "q" || line == "quit" {
			break
		}

		tokens := strings.Split(line, " ")

		debug = false
		switch tokens[0] {
		case "export":
			exportAction(tokens)
		case "debug":
			debug = true
			exportAction(tokens)
		case "import":
			importAction(tokens)
		default:
			fmt.Println("Unrecognized command: ", tokens[0])
		}
		total = 0
		totalI = 0
		maps = map[string]int{}
	}
}