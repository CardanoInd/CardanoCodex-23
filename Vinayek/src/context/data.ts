export const CLUBS: {
  title: string;
  image: string;
  perks: string[];
  type: string;
  price: string;
  id: string;
  meet_id: string;
  bg_img: string;
}[] = [
  {
    title: "Literature Club",
    image: "/theliteraryclub.jpg",
    perks: [
      "Weekly Book Review",
      "Peer Discussions",
      "Access to private books",
      "+ More",
    ],
    type: "Literature",
    price: "20000000",
    id: "literature",
    meet_id: "zfle-kanc-qd08",
    bg_img: "literature-bg.jpeg"
  },
  {
    title: "Blockchain Club",
    image: "/blockchain.png",
    perks: [
      "Peer mentorship",
      "Blockchain tech updates",
      "Exclusive events",
      "+ More",
    ],
    type: "Blockchain",
    price: "49000000",
    id: "blockchain",
    meet_id: "",
    bg_img: "blockchain-bg.jpeg"
  },
];

export const FUNDRAISING_EVENTS: {
  title: string;
  amount: string;
  address: string;
  id: string;
  description: string;
}[] = [
  {
    address:
      "addr_test1qrktshft350sjd2p8kmyvc9dcvkpyvpu4czp6pvtdkmnj9gn3mep2wjlflq29demzeq9wxy0ql9lmcfpc7jfkj6qy20sy9235q",
    amount: "15000000",
    id: "literature",
    title: "Book night",
    description: "Engage with epic stories, fuel your imagination, support literacy"
  },
  {
    address:
      "addr_test1qqgmzzmyxelxes62xwkkzu2s55ws70epja76mh72dkhw7yqn3mep2wjlflq29demzeq9wxy0ql9lmcfpc7jfkj6qy20sp5jqs5",
    amount: "10000000",
    id: "literature",
    title: "Poetry Slam",
    description: "Slam down verses, ignite creativity, fund young artists' dreams"
  },
  {
    address:
      "addr_test1qzgnrjct4garumzqetc4t4r82qfpxqdlqpxrmcknemld76sn3mep2wjlflq29demzeq9wxy0ql9lmcfpc7jfkj6qy20s0ke4h3",
    amount: "40000000",
    id: "blockchain",
    title: "Crypto for a Cause: Seminar",
    description: "Dive into crypto, empower change, back next-gen tech"
  },
  {
    address:
      "addr_test1qp0ywntphj88rxqhncd067n42wt8j9rs7mg6u25dnn7aysgn3mep2wjlflq29demzeq9wxy0ql9lmcfpc7jfkj6qy20s38fex0",
    amount: "35000000",
    id: "blockchain",
    title: "NFT & Chill",
    description: "Explore digital art, vibe with innovation, boost art tech education"
  },
];
