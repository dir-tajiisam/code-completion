// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from "next";
import * as prettier from "prettier";
import * as cadence from "prettier-plugin-cadence";

type Data = {
  code: string;
};

export default function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data>
) {
  const { body, method } = req;
  const code = body.code as string;

  if (method === "POST") {
    try {
      const text = prettier.format(code, {
        parser: "cadence",
        plugins: [cadence],
      });
      res.status(200).json({ code: text });
    } catch (error) {
      res.status(400).errored;
    }
  }
}
