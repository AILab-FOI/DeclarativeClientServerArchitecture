import { Component, computed, input } from '@angular/core';
import { Sadrzaj } from '../../core';
import { DatePipe } from '@angular/common';

@Component({
  selector: 'app-content',
  standalone: true,
  imports: [DatePipe],
  templateUrl: './content.component.html',
  styleUrl: './content.component.scss',
})
export class ContentComponent {
  public sadrzaj = input.required<Sadrzaj>();

  public datum = computed(() => {
    let date = new Date(
      parseInt(this.sadrzaj().vrijednost.vrijeme_kreiranja.toString()) * 10000,
    );
    return `${date.toLocaleDateString()} ${date.toLocaleTimeString()}`;
  });
}
