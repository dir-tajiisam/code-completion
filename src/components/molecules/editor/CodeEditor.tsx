import { Box } from "@chakra-ui/react";
import React, { FC } from "react";
import AceEditor from "react-ace";
import "ace-builds/webpack-resolver";
import "ace-builds/src-noconflict/mode-javascript";
import "ace-builds/src-noconflict/theme-tomorrow";

type CodeEditorType = {
  value: string;
  onChange?: ((value: string, event?: unknown) => void) | undefined;
  maxLines?: number;
  width?: string;
  height?: string;
};

const CodeEditor: FC<CodeEditorType> = ({
  value,
  maxLines = 30,
  onChange,
  width = "70vw",
}) => {
  const lineNum = value.split(/\r\n|\r|\n/).length;
  if (lineNum < maxLines) {
    value += "\n".repeat(maxLines - lineNum);
  }

  return (
    <Box borderWidth="1px" borderRadius="sm">
      <AceEditor
        mode="javascript"
        theme="tomorrow"
        value={value}
        onChange={onChange}
        name="UNIQUE_ID_OF_DIV"
        width={width}
        wrapEnabled
        maxLines={Number(maxLines + 1)}
        setOptions={{ useWorker: false }}
      />
    </Box>
  );
};

export default CodeEditor;
